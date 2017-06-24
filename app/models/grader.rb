class Grader < ApplicationRecord
  belongs_to :submission
  belongs_to :grader
  belongs_to :upload
  has_many :grades

  # Needed because when Cocoon instantiates new graders, it doesn't know what
  # subtype they are, yet
  attr_accessor :test_class
  attr_accessor :errors_to_show
  # Needed to make the upload not be anonymous
  attr_accessor :upload_by_user_id

  def self.unique
    select(column_names - ["id"]).distinct
  end

  def grade(assignment, submission)
    ans = do_grading(assignment, submission)
    submission.compute_grade! if submission.grade_complete?
    ans
  end

  def autograde?
    false
  end

  def autograde!(assignment, submission)
    grade(assignment, submission)
  end

  def grade_exists_for(sub)
    !Grade.find_by(grader_id: self.id, submission_id: sub.id).nil?
  end

  def ensure_grade_exists_for!(sub)
    g = grade_for(sub)
    if g.new_record?
      g.save
      true
    else
      false
    end
  end

  def upload_file
    if @upload_data
      @upload_data.original_filename
    elsif self.upload
      self.upload.file_name
    else
      nil
    end
  end

  def upload_file=(data)
    if data.nil?
      errors[:base] << "You need to submit a file."
      return
    end

    up = Upload.new
    up.user_id = self.upload_by_user_id
    up.store_upload!(data, {
                       type: "#{type} Configuration",
                       date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                       mimetype: data.content_type
                     })

    self.upload_id_will_change! if self.upload.nil? or (self.upload.id != data.id)
    self.upload = up
  end

  def assign_attributes(attrs)
    self.upload_by_user_id = attrs.delete(:upload_by_user_id)
    super(attrs)
  end
  
  protected

  def do_grading(assignment, submission)
    fail NotImplementedError, "Each grader should implement this"
  end

  def grade_for(sub)
    g = Grade.find_or_initialize_by(grader_id: self.id, submission_id: sub.id)
    if g.new_record?
      g.out_of = self.avail_score
      g.save
    end
    g
  end
end
