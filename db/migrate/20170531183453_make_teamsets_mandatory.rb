class MakeTeamsetsMandatory < ActiveRecord::Migration[5.1]
  def self.up
    courses_needing_teamsets = Course.find(Assignment.where(team_set_id: nil).map(&:course_id))
    courses_needing_teamsets.each do |c|
      ts = TeamSet.create(course: c, name: "Initial teamset for #{c.name}")
      c.assignments.where(team_set_id: nil).each do |a|
        a.update_attribute(:team_set_id, ts.id)
      end
      c.teams.where(team_set_id: nil).each do |t|
        t.update_attribute(:team_set_id, ts.id)
      end
    end
    change_column_null :teams, :team_set_id, false
    change_column_null :assignments, :team_set_id, false
  end
  def self.down
    change_column_null :teams, :team_set_id, true
    change_column_null :assignments, :team_set_id, true
  end
end
