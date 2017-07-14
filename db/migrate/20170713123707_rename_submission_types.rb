class RenameSubmissionTypes < ActiveRecord::Migration[5.1]
  def up
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'ExamSub' where type = 'Exam'
   })
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'FilesSub' where type = 'Files'
   })
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'QuestionsSub' where type = 'Questions'
   })
    change_column_default :submissions, :type, nil
  end

  def down
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'Exam' where type = 'ExamSub'
   })
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'Files' where type = 'FilesSub'
   })
    ActiveRecord::Base.connection.execute(%Q{
       update submissions set type = 'Questions' where type = 'QuestionsSub'
   })
    change_column_default :submissions, :type, "Files"
  end
end
