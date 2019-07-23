require 'rubygems/package'
require 'zlib'

class SubTarball
  def initialize(as_id)
    if as_id.is_a? Assignment
      @as = as_id
    else
      @as = Assignment.find(as_id)
    end
  end

  def update!
    make_tar do |afname, dirs|
      @as.all_used_subs.each do |sub|
        next if sub.file_full_path.blank?
        
        uu = sub.user
        dd = dirs.join("#{sub.id}_#{uu.dir_name}")
        FileUtils.mkdir_p(dd)
        
        FileUtils.cp(sub.file_full_path, dd)
      end
    end
  end

  def update_moss!
    make_tar do |afname, dirs|
      @as.all_used_subs.each do |sub|
        next if sub.upload.nil?

        uu = sub.user
        @dd = dirs.join("#{sub.id}_#{uu.dir_name}")
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
    end
  end

  def update_with!(files)
    make_tar do |afname, dirs|
      files.each do |dest, src|
        dd = dirs.join(dest)
        FileUtils.mkdir_p(dd.dirname)
        FileUtils.cp(src, dd)
      end
    end
  end

  def path
    @as.tarball_path
  end
  def full_path
    @as.tarball_full_path
  end

  protected
  def make_tar
    temp = Rails.root.join('tmp', 'tars', 'assign', @as.id.to_s)
    if temp.to_s =~ /tars\/assign/
      FileUtils.rm_rf(temp)
    end

    FileUtils.mkdir_p(temp)

    afname = "assignment_#{@as.id}"

    dirs = temp.join(afname)
    FileUtils.mkdir_p(dirs)

    # Actually fill the archive
    yield afname, dirs

    FileUtils.cd(temp)
    system(%Q{tar czf "#{afname}.tgz" "#{afname}"})

    src = temp.join("#{afname}.tgz")

    FileUtils.cp(src, @as.tarball_full_path)
    FileUtils.rm_rf(temp)
  end
end
