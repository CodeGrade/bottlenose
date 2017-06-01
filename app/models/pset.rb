class Pset < Assignment
  # These two methods are needed to help download the entire assignment's submissions as a single tarball
  def tarball_path
    if tar_key.blank?
      self.tar_key = SecureRandom.hex(16)
      save!
    end

    dir = "downloads/#{tar_key}/"
    FileUtils.mkdir_p(Rails.root.join('public', dir))

    return '/' + dir + "assignment_#{id}.tgz"
  end

  def tarball_full_path
    Rails.root.join('public', tarball_path.sub(/^\//, ''))
  end
end
