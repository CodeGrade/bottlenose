class MoveOrderToGraders < ActiveRecord::Migration[5.1]
  def up
    add_column :graders, :order, :integer
    add_column :graders, :assignment_id, :integer
    AssignmentGrader.all.each do |ag|
      ag.grader.update(order: ag.order, assignment_id: ag.assignment_id)
    end
    Grader.where(order: nil).update(order: DateTime.now.to_i)
    # diagnostics to find orphaned graders and grades
    Grader.where(assignment_id: nil).each{|g| print "#{g.id} #{g.type}\n"}
    print Grader.where(order: nil).map(&:id)
    change_column_null :graders, :order, false
    change_column_null :graders, :assignment_id, false
    drop_table :assignment_graders
  end
  def down
    create_table :assignment_graders do |t|
      t.integer :assignment_id, null: false
      t.integer :grader_id, null: false
      t.integer :order, null: false
    end
    Grader.all.group_by(&:assignment_id).each do |aid, gs|
      gs.sort_by(&:created_at).each do |g, i|
        AssignmentGrader.create!(assignment_id: aid, grader_id: g.id, order: g.order)
      end
    end
    remove_column :graders, :order
    remove_column :graders, :assignment_id
  end
end
