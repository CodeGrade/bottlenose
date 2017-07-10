class RenameAssignmentTypes < ActiveRecord::Migration[5.1]
 def up
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'Exam' where type = 'exam'
   })
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'Files' where type = 'files'
   })
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'Questions' where type = 'questions'
   })
   change_column_default :assignments, :type, nil
 end

 def down
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'exam' where type = 'Exam'
   })
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'files' where type = 'Files'
   })
   ActiveRecord::Base.connection.execute(%Q{
       update assignments set type = 'questions' where type = 'Questions'
   })
   change_column_default :assignments, :type, "files"
 end
end
