
class FakeUpload
  def initialize(path)
    @path = path
    @name = File.basename(path)
  end

  def read
    File.read(@path)
  end 

  def original_filename
    @name
  end

  def content_type
    'application/octet-stream'
  end

  def size
    File.size(@path)
  end
end
