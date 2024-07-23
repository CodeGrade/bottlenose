require 'fileutils'

class JavaGraderFileProcessor
  def self.process_zip(upload)
    grader_dir = upload.extracted_path
    starter_zip_path = grader_dir.join('starter.zip')
    testing_zip_path = grader_dir.join('testing.zip')
    # Since this method is run on a grader's :after_save hook, it's
    # possible files may already exist and need to be cleaned up.
    [starter_zip_path, testing_zip_path].each do |zip_path|
      FileUtils.rm(zip_path) if File.exist?(zip_path)
    end
    if Dir.exist?(grader_dir.join('starter')) && Dir.exist?(grader_dir.join('testing'))
      populate_zip(starter_zip_path, grader_dir, 'starter')
      populate_zip(testing_zip_path, grader_dir, 'testing')
      include_starter_zip = true
    else
      populate_zip(testing_zip_path, grader_dir, '')
      include_starter_zip = false
    end
    { testing: testing_zip_path, **include_starter_zip ? { stater: starter_zip_path } : {} }
  end

  def self.populate_zip(zip_file_path, grader_dir, grader_contents_path)
    Zip::File.open(zip_file_path, Zip::File::CREATE) do |zipfile|
      Dir[File.join(grader_dir, grader_contents_path, '**', '**')].each do |file|
        path = Pathname.new(file)
        zipfile.add(
          path.relative_path_from(File.join(grader_dir, grader_contents_path)),
          file
        )
      end
    end
  end
end

