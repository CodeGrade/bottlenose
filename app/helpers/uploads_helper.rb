require 'fileutils'
require 'audit'
require 'open3'
require 'headless'

module UploadsHelper
  class Postprocessor
    private
    PROCS = {}
    public
    def self.create_handler(name, &block)
      name = name.to_s
      # slight shenanigans to access the private :define_method and :remove_method methods
      # but this avoids the icky "warning: tried to create Proc object without a block"
      PROCS.class.send(:define_method, :temp_method, &block)
      PROCS[name] = PROCS.class.instance_method(:temp_method).bind(PROCS)
      PROCS.class.send(:remove_method, :temp_method)
    end
    def self.alias_handler(new_name, old_name)
      new_name = new_name.to_s
      old_name = old_name.to_s
      PROCS[new_name] = PROCS[old_name]
    end

    def self.process(extracted_path, f)
      ext = File.extname(f)[1..-1]
      PROCS[ext]&.call(extracted_path, f)
    end

    def self.no_files_found(extracted_path)
      File.open(extracted_path.join("no_files.txt"), "w") do |f|
        f.write "This is an automated message:\nNo files were found in this submission"
      end
    end

    create_handler :rtf do |extracted_path, f|
      return false unless (File.read(f, 6) == "{\\rtf1" rescue false)
      # Creates the path .../converted/directory/where/file/is/
      # and excludes the filename from the directory structure,
      # since only one output file is created
      converted_path = extracted_path.dirname.join("converted")
      output_path = File.dirname(f).to_s.gsub(extracted_path.to_s, converted_path.to_s)
      Pathname.new(output_path).mkpath
      output, err, status, timed_out = ApplicationHelper.capture3("soffice", "--headless",
                                                                  "--convert-to", "pdf:writer_pdf_Export",
                                                                  "--outdir", output_path,
                                                                  f,
                                                                  timeout: 30)
      if status.success? && !timed_out
        Audit.log "Successfully processed #{f} to #{output_path}"
        return true
      else
        FileUtils.rm "#{output_path}/#{File.basename(f, '.*')}.pdf", force: true
        Audit.log <<ERROR
================================
Problem processing #{f}:
Status: #{status}
Error: #{err}
Output: #{output}
================================
ERROR
        return false
      end
    end

    create_handler :doc do |extracted_path, f|
      return false unless (File.read(f, 8) == "\xD0\xCF\x11\xE0\xA1\xB1\x1A\xE1" rescue false)
      # Creates the path .../converted/directory/where/file/is/
      # and excludes the filename from the directory structure,
      # since only one output file is created
      converted_path = extracted_path.dirname.join("converted")
      output_path = File.dirname(f).to_s.gsub(extracted_path.to_s, converted_path.to_s)
      Pathname.new(output_path).mkpath
      output, err, status, timed_out = ApplicationHelper.capture3("soffice", "--headless",
                                                                  "--convert-to", "pdf:writer_pdf_Export",
                                                                  "--outdir", output_path,
                                                                  f,
                                                                  timeout: 30)
      if status.success? && !timed_out
        Audit.log "Successfully processed #{f} to #{output_path}"
        return true
      else
        FileUtils.rm "#{output_path}/#{File.basename(f, '.*')}.pdf", force: true
        Audit.log <<ERROR
================================
Problem processing #{f}:
Status: #{status}
Error: #{err}
Output: #{output}
================================
ERROR
        return false
      end
    end

    create_handler :docx do |extracted_path, f|
      return false unless (File.read(f, 4) == "\x50\x4B\x03\x04" rescue false)
      # Creates the path .../converted/directory/where/file/is/
      # and excludes the filename from the directory structure,
      # since only one output file is created
      converted_path = extracted_path.dirname.join("converted")
      output_path = File.dirname(f).to_s.gsub(extracted_path.to_s, converted_path.to_s)
      Pathname.new(output_path).mkpath
      output, err, status, timed_out = ApplicationHelper.capture3("soffice", "--headless",
                                                                  "--convert-to", "pdf:writer_pdf_Export",
                                                                  "--outdir", output_path,
                                                                  f,
                                                                  timeout: 30)
      if status.success? && !timed_out
        Audit.log "Successfully processed #{f} to #{output_path}"
        return true
      else
        FileUtils.rm "#{output_path}/#{File.basename(f, '.*')}.pdf", force: true
        Audit.log <<ERROR
================================
Problem processing #{f}:
Status: #{status}
Error: #{err}
Output: #{output}
================================
ERROR
        return false
      end
    end

    create_handler :rkt do |extracted_path, f|
      embeds_path = extracted_path.dirname.join("embedded")
      # Creates the path .../embedded/path/to/filename.rkt/embed#.png
      # includes the filename in the directory structure deliberately,
      # in case multiple racket files coexist in the same directory
      output_path = f.to_s.gsub(extracted_path.to_s, embeds_path.to_s)
      Pathname.new(output_path).mkpath
      Headless.ly(display: output_path.hash % Headless::MAX_DISPLAY_NUMBER, autopick: true) do
        output, err, status, timed_out = ApplicationHelper.capture3(
                               {"XDG_RUNTIME_DIR" => nil},
                               "racket", Rails.root.join("lib/assets/render-racket.rkt").to_s,
                               "-e", output_path,
                               "-o", f + "ext",
                               f,
                               timeout: 30)
        if status.success? && !timed_out
          contents = File.read(f + "ext")
          File.open(f, "w") do |f|
            f.write contents.gsub(Upload.base_upload_dir.to_s, "/files")
          end
          FileUtils.rm(f + "ext")
          Audit.log "Successfully processed #{f} to #{output_path}"
          return true
        else
          FileUtils.rm (f + "ext"), force: true
          Audit.log <<ERROR
================================
Problem processing #{f}:
Status: #{status}
Error: #{err}
Output: #{output}
================================
ERROR
          return false
        end
      end
    end
    alias_handler :ss, :rkt
  end
end
