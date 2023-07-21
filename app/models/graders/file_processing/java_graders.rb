require 'fileutils'

class JavaGraderFileProcessor

  def self.process_zip(upload, grader)
    grader_path = upload.grader_path(grader)
    starter_zip_path, testing_zip_path = grader_path.join("starter.zip"),
                                          grader_path.join("testing.zip")
    if Dir.exists?(grader_path.join("starter")) && Dir.exists?(grader_path.join("testing"))
      self.populate_zip(starter_zip_path, grader_path, "starter")
      self.populate_zip(testing_zip_path, grader_path, "testing")
    else
      FileUtils.touch(starter_zip_path)
      self.populate_zip(testing_zip_path, grader_path, "")
    end
  end

  def self.populate_zip(zip_file_path, grader_path, grader_contents_path)
    Zip::File.open(zip_file_path, Zip::File::CREATE) do |zipfile|
      Dir[File.join(grader_path, grader_contents_path, "**", "**")] do |file|
        path = Pathname.new(file)
        zipfile.add(
          path.relative_path_from(Pathname.join(grader_path, grader_contents_path)),
          file
        )
      end
    end
  end

end