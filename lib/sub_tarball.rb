require 'rubygems/package'
require 'zlib'

class SubTarball
  def initialize(as_id)
    @as = Assignment.find(as_id)
  end

  def update!
    temp = Rails.root.join('tmp', 'tars', 'assign', @as.id.to_s)
    if temp.to_s =~ /tars\/assign/
      FileUtils.rm_rf(temp)
    end

    FileUtils.mkdir_p(temp)

    afname = "assignment_#{@as.id}"

    dirs = temp.join(afname)
    FileUtils.mkdir_p(dirs)

    @as.all_used_subs.each do |sub|
      next if sub.file_full_path.blank?

      uu = sub.user
      dd = dirs.join(uu.dir_name)
      FileUtils.mkdir_p(dd)

      FileUtils.cp(sub.file_full_path, dd)
    end

    FileUtils.cd(temp)
    system(%Q{tar czf "#{afname}.tgz" "#{afname}"})

    src = temp.join("#{afname}.tgz")

    FileUtils.cp(src, @as.tarball_full_path)
  end

  def update_moss!
    temp = Rails.root.join('tmp', 'tars', 'assign', @as.id.to_s)
    if temp.to_s =~ /tars\/assign/
      FileUtils.rm_rf(temp)
    end

    FileUtils.mkdir_p(temp)

    afname = "assignment_#{@as.id}"

    dirs = temp.join(afname)
    FileUtils.mkdir_p(dirs)


    @as.all_used_subs.each do |sub|
      next if sub.upload.nil?

      uu = sub.user
      @dd = dirs.join(uu.dir_name)
      FileUtils.mkdir_p(@dd)

      def copy_files(extracted)
        if extracted[:children]
          extracted[:children].each {|c| copy_files c}
        else
          FileUtils.cp(extracted[:full_path],
                       @dd.join(extracted[:full_path].to_s.gsub(/.*extracted\//, "").gsub("/", "_").gsub(" ", "_")))
        end
      end
      sub.upload.extracted_files.each {|e| copy_files e}
    end

    
    FileUtils.cd(temp)
    system(%Q{tar czf "#{afname}.tgz" "#{afname}"})
    
    src = temp.join("#{afname}.tgz")

    FileUtils.cp(src, @as.tarball_full_path)    
  end

  def path
    @as.tarball_path
  end

  def self.untar(source, dest)
    self.untar_source(File.open(source), dest)
  end

  def self.untar_gz(source, dest)
    self.untar_source(Zlib::GzipReader.open(source), dest)
  end

private
  def self.untar_source(source, destination)
  end
end
