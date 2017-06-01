class FixAsgnType < ActiveRecord::Migration[5.1]
  def up
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'Exam' where type = 'exam'
    })
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'Pset' where type = 'files'
    })
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'Survey' where type = 'questions'
    })
  end

  def down
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'exam' where type = 'Exam'
    })
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'files' where type = 'Pset'
    })
    ActiveRecord::Base.connection.execute(%Q{
        update assignments set type = 'questions' where type = 'Survey'
    })
  end
end
