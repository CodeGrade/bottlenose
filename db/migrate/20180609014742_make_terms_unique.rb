class MakeTermsUnique < ActiveRecord::Migration[5.2]
  def change
    add_index :terms, [:semester, :year], unique: true
  end
end
