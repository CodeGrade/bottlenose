require 'test_helper'
require 'schema_checker'

class AssignmentTest < ActiveSupport::TestCase
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
- BadAnswerType:
    - MultipleChoice: oops
...
YAML
    inputs << <<-YAML
---
- MissingFields:
    - MultipleChoice: 
        weight: 5
...
YAML
    inputs << <<-YAML
---
- MissingFields:
    - MultipleChoice: 
        correctAnswer: 5
...
YAML
    inputs << <<-YAML
---
- WrongTypeMissingFields:
    - MultipleChoice: 
        weight: true
...
YAML
    inputs << <<-YAML
---
- UnknownKey:
    - MultipleChoice: 
        name: MyQuestion
        woof: 5
...
YAML
    inputs << <<-YAML
---
- NonStringKey:
    - MultipleChoice: 
        5: true
...
YAML
    inputs << <<-YAML
---
- UnknownQuestion:
    - SomeWeirdQuestion: 
        5: true
...
YAML
    inputs << <<-YAML
---
- NonStringQuestion:
    - 42: 
        5: true
...
YAML
    inputs << <<-YAML
---
- IntegerRubric:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: false
        prompt: Hi
        rubric: 42
...
YAML
    inputs << <<-YAML
---
- MalformedRubric:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 0
        prompt: Hi
        rubric:
          - foo: bar
            baz: quux
...
YAML
    inputs << <<-YAML
---
- TooManyKeysInRubric:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 0
        prompt: Hi
        rubric:
          - 1: bar
            0.5: quux
...
YAML
    inputs << <<-YAML
---
- AnswerOutOfRange:
    - MultipleChoice: 
        name: MyQuestion
        weight: 5
        correctAnswer: 666
        prompt: Hi
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
- KeysOutOfRange:
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
    inputs << <<-YAML
---
- BadBoolean:
    - YesNo:
        name: MyQuestion
        weight: 5
        correctAnswer: disagree
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
...
YAML
    inputs << <<-YAML
---
- BadBoolean:
    - TrueFalse:
        name: MyQuestion
        weight: 5
        correctAnswer: disagree
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
...
YAML
    inputs << <<-YAML
---
- BadNumeric:
    - Numeric:
        name: MyQuestion
        weight: 5
        min: true
        max: hello
        correctAnswer: [1,2,3]
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
...
YAML
    inputs << <<-YAML
---
- BadNumericAnswer:
    - Numeric:
        name: MyQuestion
        weight: 5
        min: 0
        max: 5
        correctAnswer: 10
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
...
YAML
    inputs << <<-YAML
---
- BadParts:
    - Numeric:
        name: MyQuestion
        weight: 5
        min: 0
        max: 5
        correctAnswer: 5
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
        parts: true
...
YAML
    inputs << <<-YAML
---
- BadParts:
    - Numeric:
        name: MyQuestion
        weight: 5
        min: 0
        max: 5
        correctAnswer: 5
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
        parts: 
          - true
          - false
...
YAML
    inputs << <<-YAML
---
- BadParts:
    - Numeric:
        name: MyQuestion
        weight: 5
        min: 0
        max: 5
        correctAnswer: 5
        prompt: Hi
        rubric:
          - 0: foo
          - 0.5: bar
          - 1: baz
        parts: 
          - [5]
          - unknown: hello
...
YAML

    sc = SchemaChecker.new(Rails.root.join("app/helpers/questions_schema.yaml"))
    inputs.each_with_index do |input, n|
      assert_not_equal [], sc.check(YAML.load(input))
    end
    valid_file = YAML.load(File.read(Rails.root.join("test/fixtures/files/peer-eval.yaml")))
    assert_equal [], sc.check(valid_file)
  end
end
