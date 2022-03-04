# Grading Conflicts

**NOTE:** _Grader_ in the context of this document refers to a course grader/assistant -- a.k.a., a user who can grade a
student's assignment. This is not the same as a StyleGrader, ManualGrader, etc.

## Purpose

A `GradingConflict` is a model developed to represent the existence of a conflict between a course grader/assistant 
and a student. This is used to in conjunction with operations allocating graders to student submissions, in that a grader should
NOT grade a student's submission if a conflict exists between them and the student.

## Data Definition

A `GradingConflict` contains the following data (fields):

- `course`: The `Course` associated with the conflicted student and staff
- `staff`: A `User` who is a Grader/Assistant for the course
- `student`: A `User` who is a Student in the course
- `status`: Status determining if the `GradingConflict` is in use/waiting approval. a `status` is an enumerable of either:
    
    - `:inactive` - Not used when determining grader allocations.
    - `:active` - Used when determining grader allocations.
    - `:pending` - Awaiting approval by a course professor or site admin.
    
-  `activity`: JSON used to log changes to the `GradingConflict`; more on this below.

`GradingConflict` models also have timestamps for when they are created and updated.

### The `activity` Field

TODO

### `status` Rules/Invariants

TODO

## Usage