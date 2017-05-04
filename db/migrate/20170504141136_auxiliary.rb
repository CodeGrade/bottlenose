class Auxiliary < ActiveRecord::Migration[4.2]
	def self.up
		execute "TRUNCATE schema_migrations;"
		execute "INSERT INTO schema_migrations VALUES ('20170502174648');"
	end
	def self.down
		raise ActiveRecord::IrreversibleMigration
	end
end
