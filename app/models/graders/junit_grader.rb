require 'open3'
require 'tap_parser'
require 'audit'
require 'net/http'
require 'securerandom'

class JunitGrader < Grader
  after_initialize :load_junit_params
  after_save :process_grader_zip
  before_validation :set_junit_params
  validate :proper_configuration

  @@resource_files = {
    "lib/assets/annotations.jar": ["annotations-jar", "application/java-archive", false],
    "lib/assets/junit-4.13.2.jar": ["junit-jar", "application/java-archive", false],
    "lib/assets/junit-tap.jar": ["junit-tap-jar", "application/java-archive", false],
    "lib/assets/hamcrest-core-1.3.jar": ["hamcrest-jar", "application/java-archive", false]
  }
  @@dockerfile_sha = "74b320b733359ebfa19428c8f073de1a700b6883e84ca70d0f13858830a7b250"

  def autograde?
    true
  end

  def display_type
    "JUnit Tests"
  end
  
  def to_s
    if self.upload
      filename = self.upload.file_name
    else
      filename = "<no file>"
    end
    "#{self.avail_score} points: Run JUnit tests in #{test_class} from #{filename} " +
      "(with default timeout #{test_timeout}), " +
      "and show #{pluralize(errors_to_show, 'failed test')}"
  end

  def export_data
    export_tap_data
  end
  def export_data_schema
    "junit_export_schema"
  end
  def import_data(who_grades, file)
    import_tap_data(who_grades, file) do |g, raw_tap, sub|
      if File.extname(g.grading_output_path) == ".tap"
        tap = TapParser.new(raw_tap)
        g.score = tap.points_earned
        g.out_of = tap.points_available
      else
        g.score = 0
        g.out_of = self.avail_score
      end
      g.updated_at = DateTime.now
      g.available = true
    end
  end
  def import_data_schema
    "junit_import_schema"
  end

  def grade(assignment, submission, prio = 0)
    begin
      secret, secret_file_path = generate_orca_secret!(submission)
      send_job_to_orca(submission, secret)
    rescue OrcaJobCreationError => exception
      FileUtils.rm(secret_file_path)
      Audit.log("Failed to send job to orca on submission #{submission.id} for assignment #{assignment.id}.")
    end
    super(assignment, submission, prio)
  end

  def send_job_to_orca(submission, secret)
    job_json = JSON.generate(generate_grading_job(submission, secret))
    uri = URI.parse("#{Settings["orca_url"]}/grading_queue")
    post_job_json_with_retry(job_json, uri)
  end

  def generate_grading_job(sub, secret)
    ans = {}
    ans["key"] = JSON.generate({ grade_id: self.grade_for(sub).id, secret: secret })
    ans["files"] = self.generate_file_hash
    ans["collation"] = sub.team ? { id: sub.team.id, type: "team" } :
                        { id: sub.user.id, type: "user"}
    ans["response_url"] = "#{Settings['site_url']}/"\
                          "#{orca_response_course_assignment_submission_grades(@assignment.course, @assignment, sub)}"
    ans["script"] = self.get_grading_script
    ans["grading_image_sha"] = @@dockerfile_sha
    ans["metadata"] = self.generate_grading_job_metadata_table(sub)
    ans["priority"] = self.delay_for_sub(sub).in_seconds
    ans
  end

  protected
  def load_junit_params
    return if new_record?
    testClass, errorsToShow, testTimeout = self.params.to_s.split(";")
    self.test_class = testClass
    self.errors_to_show = errorsToShow.to_i
    self.test_timeout = (testTimeout || Grader::DEFAULT_TEST_TIMEOUT).to_i
  end
  def set_junit_params
    self.params = "#{self.test_class};#{self.errors_to_show};#{self.test_timeout}"
  end
  
  def do_grading(assignment, sub)
    g = self.grade_for sub
    Dir.mktmpdir("grade-#{sub.id}-#{g.id}_") do |build_dir|
      @build_dir = build_dir
      begin
        run_build_produce_problems assignment, sub, include_dirtree: !self.test_class.downcase.include?("examplar") do |prefix, any_problems, details|
          if any_problems
            g.grading_output_path = details.path
            g.score = 0
            g.out_of = self.avail_score
            g.updated_at = DateTime.now
            g.available = true
            g.save!            
          else
            timeout = Grader::DEFAULT_GRADING_TIMEOUT
            # If the professor supplied a per-test timeout, then we can relax our overall timeout a bit
            if self.test_timeout then timeout = timeout * 5 end
            run_command_produce_tap assignment, sub, timeout: timeout do |prefix, err, g, tap|
              if tap
                g.score = tap.points_earned
                g.out_of = tap.points_available
                g.updated_at = DateTime.now
                g.available = true
                g.save!
                Audit.log("#{prefix}: Tests give raw score of #{g.score} / #{g.out_of}")
              else
                if err[:output]
                  details.write("Test output: (exit status #{err[:status] || 'unknown'})\n")
                  details.write(err[:output])
                end
                if err[:timed_out]
                  details.write("Running tests timed out after #{self.test_timeout} seconds")
                end
                Audit.log("#{prefix}: Errors prevented grading; giving a 0")
                g.grading_output_path = details.path
                g.score = 0
                g.out_of = self.avail_score
                g.updated_at = DateTime.now
                g.available = true
                g.save!
              end
            end
          end
        end
      rescue Exception => e
        Audit.log("Assignment #{assignment.id}, submission #{sub.id}: Errors prevented grading; giving a 0: #{e} at #{e.backtrace.join("\n")}")
        g.score = 0
        g.out_of = self.avail_score
        g.updated_at = DateTime.now
        g.available = true
        g.save!
      end
    end
  end

  def get_extraction_arguments(assignment, sub)
    [
      @build_dir,
      self.upload.extracted_path,
      sub.upload.extracted_path,
      [
        Rails.root.join("lib/assets/annotations.jar"),
        Rails.root.join("lib/assets/junit-4.13.2.jar"),
        Rails.root.join("lib/assets/junit-tap.jar"),
        Rails.root.join("lib/assets/hamcrest-core-1.3.jar")
      ]
    ]
  end

  def get_build_arguments(assignment, sub)
    test_classes = self.test_class.gsub(".", "/").split(" ").map{|f| ["#{@build_dir}/#{f}.java", true]}.to_h
    test_files, student_files = Dir.glob("#{@build_dir}/**/*.java").reject do |f|
      Pathname.new(f).ascend.any?{|c| c.basename.to_s == "__MACOSX" || c.basename.to_s == ".DS_STORE"}
    end.partition{|f| test_classes[f]}
    files = test_files + student_files
    File.open("#{@build_dir}/compile_list.txt", "w") do |compile_list|
      files.each do |f| compile_list.puts(f.gsub("#{@build_dir}/", "")) end
    end
    [
      "details.log",
      {},
      [
        ["cat", "compile_list.txt"],
        ["javac", "-cp", "junit-4.13.2.jar:junit-tap.jar:hamcrest-core-1.3.jar:annotations.jar:.:./*",
         "@compile_list.txt"]
      ],
      {},
      @build_dir
    ]
  end

  def get_command_arguments(assignment, sub)
    test_class_args = self.test_class.split(" ")
    test_class_args.unshift "edu.neu.TAPRunner" unless (test_class_args[0] == "examplar.Main")
    [
      "junit.tap",
      {},
      ["java", "-cp", "junit-4.13.2.jar:junit-tap.jar:hamcrest-core-1.3.jar:annotations.jar:.:./*",
       *test_class_args,
       "-timeout", self.test_timeout.to_s],
      {},
      @build_dir
    ]
  end

  def recompute_grades
    # nothing to do:
    # we already compute the score here based on the TAP output
  end

  def proper_configuration
    if self.upload.nil?
      add_error("Upload cannot be nil")
    else
      if self.test_class.blank?
        add_error("Test class cannot be blank")
      end
      if self.errors_to_show.blank?
        add_error("Errors to show cannot be blank")
      end
      if self.test_timeout.blank?
        add_error("Default test timeout cannot be blank")
      end
    end
    return if self.upload.nil? || self.test_class.blank?
    begin
      entries = self.upload.upload_entries
      if entries["#{self.test_class}.java"]
        # ok, nothing to do
      elsif entries["starter"] && entries["testing"]
        ok = true
        if entries["starter"].size > 0 && !(entries["starter"]["src"] && entries["starter"]["test"])
          add_error("The starter/ directory does not contain src/ and test/ subdirectories")
          ok = false
        end
        if !(entries["testing"]["src"] && entries["testing"]["test"])
          add_error("The testing/ directory does not contain src/ and test/ subdirectories")
          ok = false
        end
        if ok
          self.test_class.split.each do |tc|
            next if (tc.starts_with?("-") || (Float(tc) rescue false))
            if !entries["testing"]["test"]["#{tc}.java"]
              add_error("There is no #{tc}.java file to match the specified test class")
            end
          end
          if entries["testing"]["test"]["GradingSandbox.java"]
            add_error("There must not be a class named GradingSandbox")
          end
        end
      else
        ok = true
        if !(entries["src"] && entries["test"])
          add_error("The archive does not contain src/ and test/ subdirectories")
          ok = false
        end
        if ok && !self.test_class.downcase.include?("examplar")
          self.test_class.split.each do |tc|
            next if (tc.starts_with?("-") || (Float(tc) rescue false))
            if !entries["test"]["#{tc}.java"]
              add_error("There is no #{tc}.java file to match the specified test class")
            end
          end
          if entries["test"]["GradingSandbox.java"]
            add_error("There must not be a class named GradingSandbox")
          end
        end
      end
    rescue Exception => e
      e_msg = e.to_s
      e_msg = e_msg.dump[1...-1] unless e_msg.is_utf8?
      add_error("Could not read upload: #{e_msg}")
    end
  end

  # Orca grading job methods.

  # Generates a secret to be paired with an Orca grading job
  # and compared upon response. Returns the secret and the
  # file_path to which it was saved.
  def generate_orca_secret!(sub)
    grader_dir = sub.upload.grader_path(self)
    secret = SecureRandom.hex(32) 
    file_path = grader_dir.join("orca.secret")
    File.open(file_path, "w") do |secret_file|
      secret_file.write(secret)
    end
    return secret, file_path
  end

  def post_job_json_with_retry(uri, job_json)
    # Exponential back off variables. Wait time in ms.
    max_wait_time, current_exponent, status_code = 32 * 1000, 0, nil
    while status_code != 200 do
     unless status_code == nil
       random_wait_interval = rand(1000)
       wait_time = [(2**current_exponent * 1000) + random_wait_interval, max_wait_time].min
       if wait_time == max_wait_time
         break
       end
       sleep(wait_time)
     end
    end

    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    res = http.post(job_json, {"Content-Type": "application/json"})
    status_code = res.status_code
    current_exponent++

    if status_code != 200
      raise OrcaJobCreationError.new(status_code)
    end
 end

  def generate_files_hash(sub)
    files = {}

    files["sub"] = {}
    files["sub"]["url"] = sub.upload.url
    files["sub"]["mime_type"] = sub.upload.read_metadata["mimetype"].first
    files["sub"]["should_replace_paths"] = false

    @zip_paths.each do |key, path|
      files[key] = {}
      files[key]["url"] = Settings["site_url"] + Upload.upload_path_for(path)
      files[key]["mime_type"] = "application/zip"
      files[key]["should_replace_paths"] = false
    end

    # TODO: configure separation of starter and test and rewrite this logic
    files["grader_zip"] = {}
    files["grader_zip"]["url"] = self.upload.url
    files["grader_zip"]["mime_type"] = self.upload.read_metadata["mimetype"].first
    files["grader_zip"]["should_replace_paths"] = false
    
    @@resource_files.each do |file_path, (files_key, mime, should_replace_paths)|
      files[files_key] = {}
      files[files_key]["url"] = 
        "#{Settings.site_url}/resources/#{files_path.gsub('lib/assets/', '')}"
      files[files_key]["mime_type"] = mime
      files[files_key]["should_replace_paths"] = should_replace_paths
    end
    files
  end

  def get_grading_script
    build_script = JSON.load "lib/assets/build-scripts/junit_grader.json"
    build_script << {
      cmd: ["java", "-cp", "junit-4.13.2.jar:junit-tap.jar:hamcrest-core-1.3.jar:annotations.jar:.:./*",
        "edu.neu.TAPRunner", *(self.test_class.split(" ")),
        "-timeout", self.test_timeout.to_s],
      on_complete: "output",
      timeout: 360,
      working_dir: "$BUILD"
    }
  end

  def process_grader_zip
    zip_paths = JavaGraderFileProcessor.process_zip(self.upload, self)
    @zip_paths = zip_paths
  end

  class OrcaJobCreationError < StandardError
    
    attr_reader :status_code

    def initialize(status_code, msg="Could not send a job to Orca after multiple HTTP request retries.")
      @status_code = status_code
      super
    end
  end

end
