require 'open3'
require 'tap_parser'
require 'audit'
require 'net/http'
require 'digest'
require 'securerandom'

class JunitGrader < Grader
  after_initialize :load_junit_params, :create_image_build_config
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
    if self.test_class&.downcase.include?("examplar")
      "Examplar results"
    else
      "JUnit Tests"
    end
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
      g.updated_at = DateTime.current
      g.available = true
    end
  end
  def import_data_schema
    "junit_import_schema"
  end

  def grade(assignment, submission, prio = 0)
    Thread.new do
      grader_dir = submission.upload.path_for(self)
      begin
        secret, secret_file_path = generate_orca_secret!(submission, grader_dir)
        send_job_to_orca(submission, secret)
      rescue IOError => e
        error_str = "Failed to create secret for job; encountered the following: #{e}"
        write_failed_job_result(error_str, grader_dir)
      rescue Net::HTTPError => e
        FileUtils.rm(secret_file_path)
        error_str = "Failed to send job to Orca; enountered the following: #{e}"
        write_failed_job_result(error_str, grader_dir)
      end
    end
    super(assignment, submission, prio)
  end

  def send_job_to_orca(submission, secret)
    orca_url = orca_config['site_url'][Rails.env]
    job_json = JSON.generate(generate_grading_job(submission, secret))
    uri = URI.parse("#{orca_url}/grading_queue")
    put_job_json_with_retry!(uri, job_json)
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

  def check_for_malformed_submission(upload)
    errors = []
    entries = upload.upload_entries
    if entries["src"] && !entries["test"]
      errors << "Having a src/ directory requires having a test/ directory also"
    end
    if entries["test"] && !entries["src"]
      errors << "Having a test/ directory requires having a src/ directory also"
    end
    if entries.size == 1
      rootName, rootEntry = entries.first
      if rootEntry == true
        # nothing to do; it's just one file
      elsif rootName != "src" && rootName != "test"
        errors << "Make sure your submission is directly at the root of your zip, not nested within another directory (#{rootName}/)"
      end
    end
    if self.test_class&.downcase.include?("examplar")
      if entries["src"].is_a?(Hash)
        java_files = search_for(entries["src"], "src") {|f| f.ends_with? ".java" }
        if java_files.size > 0
          errors << "Make sure not to submit any src/ files for Examplar assignments (#{java_files.to_sentence})"
        end
      end
    end
    errors
  end

  def create_image_build_config
    save_dir = upload.grader_dir(self)
    File.open(save_dir.join('image-config.json'), 'w') do |f|
      f.write(image_build_config)
    end
  end

  def image_build_config
    # TODO: This should be saved to the assets or resources dir.
    dockerfile_path = Rails.root.join("app/models/graders/dockerfiles/orca-java-grader.Dockerfile")
    File.open(dockerfile_path) do |dockerfile_fp|
      contents = dockerfile_fp.read
      sha_sum = Digest::SHA256.base64digest
      return {
        dockerfileContents: contents,
        dockerfileSHASum: sha_sum
      }
    end
  end

  protected
  def search_for(entries, path, &block)
    entries.map do |k, v|
      filename = "#{path}#{File::SEPARATOR}#{k}"
      if v == true
        if block.call(filename)
          [filename]
        else
          []
        end
      else
        search_for(v, filename, &block)
      end
    end.flatten
  end

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
            g.updated_at = DateTime.current
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
                g.updated_at = DateTime.current
                g.available = true
                g.save!
                Audit.log("#{prefix}: Tests give raw score of #{g.score} / #{g.out_of}")
              else
                if err[:output]
                  details.write("Test output: (exit status #{err[:status] || 'unknown'})\n")
                  details.write(err[:output])
                end
                if err[:err]
                  details.write("Test errored: (exit status #{err[:status] || 'unknown'})\n")
                  details.write(err[:err])
                end
                if err[:timed_out]
                  details.write("Running tests timed out after #{self.test_timeout} seconds")
                end
                Audit.log("#{prefix}: Errors prevented grading; giving a 0")
                g.grading_output_path = details.path
                g.score = 0
                g.out_of = self.avail_score
                g.updated_at = DateTime.current
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
        g.updated_at = DateTime.current
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
            if !classNamed(entries["testing"]["test"], tc)
              add_error("There is no #{tc} file (either .java or .class) to match the specified test class")
            end
          end
          if classNamed(entries["testing"]["test"], "GradingSandbox")
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
            if !classNamed(entries["test"], tc)
              add_error("There is no #{tc} file (either .java or .class) to match the specified test class")
            end
          end
          if classNamed(entries["test"], "GradingSandbox")
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
  def generate_orca_secret!(sub, secret_save_dir)
    secret_length = 32
    secret = SecureRandom.hex(secret_length)
    file_path = secret_save_dir.join("orca.secret")
    File.open(file_path, "w") do |secret_file|
      secret_file.write(secret)
    end
    [secret, file_path]
  end

  def write_failed_job_result(error_str, result_dir)
    result = generate_failed_job_result(error_str)
    File.open(result_dir.join('result.json'), 'w') do |f|
      f.write(result.to_json)
    end
  end

  def generate_failed_job_result(error_str)
    {
      type: "GradingJobResult",
      shell_responses: [],
      errors: [error_str]
    }
  end

  def put_job_json_with_retry!(uri, job_json)
    # Exponential back off variables. Wait time in ms.
    max_requests = 5
    attempts, status_code = 0, nil
    while status_code != 200
      response = Net::HTTP.put(uri, job_json, { 'Content-Type' => 'application/json' })
      status_code = response.code
      if should_retry_web_request? status_code
        attempts += 1
        if attempts == max_requests then break end
        sleep(2**attempts + rand)
      end
    end
    response.value
  end

  def should_retry_web_request?(status_code)
    [503, 504].include? status_code
  end

  def generate_files_hash(sub)
    files = {
      submission: {
        url: sub.upload.url,
        mime_type: sub.upload.read_metadata["mimetype"].first,
        should_replace_paths: false
      }
    }

    @zip_paths.each do |key, path|
      files[key] = {}
      files[key]["url"] = Settings["site_url"] + Upload.upload_path_for(path)
      files[key]["mime_type"] = "application/zip"
      files[key]["should_replace_paths"] = false
    end

    # TODO: configure separation of starter and test and rewrite this logic
    files["grader_zip"] = {
      url: self.upload.url,
      mime_type: self.upload.read_metadata["mimetype"].first,
      should_replace_paths: false
    }

    @@resource_files.each do |file_path, (files_key, mime, should_replace_paths)|
      files[files_key] = {
        url: "#{Settings.site_url}/resources/#{files_path.gsub('lib/assets/', '')}",
        mime_type: mime,
        should_replace_paths: should_replace_paths
      }
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
    @zip_paths = JavaGraderFileProcessor.process_zip(upload, self)
  end

  def classNamed(dict, name)
    sources = name.split(".")
    sources[-1] += "\.java"
    classes = name.split(".")
    classes[-1] += ".class"
    return dict.dig(*sources) || dict.dig(*classes)
  end
end
