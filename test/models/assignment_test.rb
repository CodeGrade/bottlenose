require 'test_helper'
require 'schema_checker'

class AssignmentTest < ActiveSupport::TestCase
  include AssignmentsHelper
  attr_accessor :assignment_upload
  attr_accessor :errors
  test "invalid questions" do
    inputs = []
    inputs << <<-YAML
---
NotAnArray: true
...
YAML
    inputs << <<-YAML
---
- NotAValidSection
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    Not: an array of questions
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - Not: an array of questions
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: oops
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        weight: 5
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        weight: true
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        name: MyQuestion
        woof: 5
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        5: true
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - SomeWeirdQuestion: 
        5: true
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - 42: 
        5: true
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: false
        rubric: 42
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 0
        rubric:
          - foo: bar
            baz: quux
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 666
        options:
          - Foo
          - Bar
        rubric:
          - foo: bar
            baz: quux
...
YAML
    inputs << <<-YAML
---
- FirstSection:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 666
        options:
          - Foo
          - Bar
        rubric:
          - 0: foo
            0.5: bar
            1: baz
            1.5: quux
...
YAML

    sc = SchemaChecker.new(Rails.root.join("app/helpers/questions_schema.yaml"))
    inputs.each_with_index do |input, n|
      self.errors = ActiveModel::Errors.new(nil)
      @assignment_file_data = FakeUpload.new("test #{n}", input)
      assert_not check_questions_schema
      puts "Existing:"
      puts self.errors.full_messages
      puts "New:"
      puts sc.check(YAML.load(input))
      puts
    end
  end
end
