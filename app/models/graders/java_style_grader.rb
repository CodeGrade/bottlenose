require 'open3'
require 'tap_parser'
require 'audit'

class JavaStyleGrader < Grader
  def autograde?
    true
  end

  def display_type
    "Java Style"
  end
  
  def to_s
    if self.upload
      "#{self.avail_score} points: Run Java style checker, using #{self.upload.file_name}"
    else
      "#{self.avail_score} points: Run Java style checker"
    end
  end

  def export_data
    export_tap_data
  end
  def export_data_schema
    "tap_style_export_schema.html"
  end
  def import_data(who_grades, file)
    import_tap_data(who_grades, file)
  end
  def import_data_schema
    "tap_style_import_schema.html"
  end

  validate :proper_configuration
  
  protected
  
  def do_grading(assignment, sub)
    run_command_produce_tap assignment, sub, timeout: Grader::DEFAULT_GRADING_TIMEOUT do |prefix, err, g, tap|
      if err
        record_compile_error(sub, g)
      else
        record_tap_as_comments(g, tap, sub)
      end
    end
  end
  def get_command_arguments(assignment, sub)
    files_dir = sub.upload.extracted_path
    ans = [
      "style.tap",
      {},
      ["java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
       files_dir.to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/junit-4.12.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/hamcrest-core-1.3.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/annotations.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/mockito-core-2.0.7-beta.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/tester-3.0.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/javalib.jar").to_s,
       "-maxPoints", self.avail_score.to_s],
      [[files_dir.to_s, Upload.upload_path_for(files_dir.to_s)]].to_h
    ]
    if self.upload&.submission_path && File.file?(self.upload.submission_path)
      ans[2].insert(4, 
         "+config", self.upload.submission_path.to_s)
    end
    ans
  end

  def recompute_grades
    # nothing to do:
    # we already compute the score here based on the TAP output
  end


  KNOWN_CATEGORIES = [
    "default",
    "naming", "namingWarning",
    "formatting", "formattingWarning",
    "coding", "codingMinor", "codingWarning",
    "documentation",
    "testing", "testingMinor",
    "suggestion",
    "ignore"
  ].map!(&:downcase)
  def proper_configuration
    return if self.upload.nil?
    begin
      contents = self.upload.upload_data
      json = JSON.parse(contents)
      if !json.is_a? Hash
        add_error("configuration file is not a valid dictionary")
      else
        json.each do |k, v|
          case k
          when "maximum deductions per file"
            if !is_int(v)
              add_error("maximum deductions per file is not an integer")
            end
          when "initial points"
            if !is_float(v)
              add_error("maximum deductions per file is not an integer")
            end
          else
            if v.is_a? Hash
              new_v = v.map{|k, v| [k.downcase, v]}.to_h
              if new_v.size != v.size
                add_error("Key #{k} maps to a dictionary with two keys that are the same, case-insensitively")
              elsif new_v["category"].blank?
                add_error("Key #{k} doesn't specify what category of issue it is")
              end
              new_v.each do |vk, vv|
                case vk.downcase
                when "category"
                  if json[vv].nil? && !KNOWN_CATEGORIES.member?(vv.downcase)
                    add_error("Key #{k} maps to category #{vv}, which doesn't exist")
                  end
                when "description"
                  if !vv.is_a? String
                    add_error("Key #{k} has a non-string description #{vv}")
                  end
                when "severity"
                  if !vv.is_a? String
                    add_error("Key #{k} has a non-string severity #{vv}")
                  elsif !["error", "warning", "suggestion", "info", "ignore"].member? vv.downcase
                    add_error("Key #{k} has an unknown severity #{vv}")
                  end
                when "deduction"
                  if !is_int(vv)
                    add_error("Key #{k} has a non-integer deduction #{vv}")
                  end
                when "maximumdeductions"
                  if !is_int(vv)
                    add_error("Key #{k} has a non-integer maximumDeductions #{vv}")
                  end
                else
                  add_error("Key #{k} has an unknown attribute #{vk} => #{vv}")
                end
              end
            else
              add_error("Key #{k} isn't mapped to a dictionary of options")
            end
          end
        end
      end
    rescue Exception => e
      e_msg = e.to_s
      e_msg = e_msg.dump[1...-1] unless e_msg.is_utf8?
      add_error("configuration file is invalid JSON: #{e_msg}")
    end
    return self.errors.empty?
  end
end
