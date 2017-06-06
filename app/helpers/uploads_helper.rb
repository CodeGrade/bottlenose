require 'fileutils'
require 'audit'
require 'open3'

module UploadsHelper
  class Postprocessor
    def self.process(f)
      if self.respond_to?(File.extname(f)[1..-1])
        self.method(File.extname(f)[1..-1]).call(f)
      end
    end

    def self.rkt(f)
      output, err, status = Open3.capture3("xvfb-run", "-a", "--server-num", "1",
                                           "racket", Rails.root.join("lib/assets/render-racket.rkt").to_s,
                                           "-o", f + "ext",
                                           f)
      if status.success? || File.exists?(f + "ext")
        FileUtils.mv f + "ext", f
        Audit.log "Successfully processed #{f}"
      else
        FileUtils.rm f + "ext", force: true
        Audit.log <<ERROR
================================
Problem processing #{f}:
Status: #{status}
Error: #{err}
Output: #{output}
================================
ERROR
      end
    end
    singleton_class.send(:alias_method, :ss, :rkt)
  end
end
