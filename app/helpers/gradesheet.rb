class Gradesheet
  attr_reader :missing_grades
  attr_reader :assignment
  attr_reader :submissions
  attr_reader :graders
  attr_reader :raw_grades
  attr_reader :max_score
  attr_reader :grades
  def initialize(assignment, submissions)
    # A Grader specifies its relative weight compared to the other graders for that assignment

    # A Grade specifies a score and out_of, both of which are points.

    # Therefore, an assignment's grade is
    # Sum_{g in relevant grades} ((g.score / g.out_of) * g.grader.avail_score)
    # -------------------------------------------------------------------------
    # Sum_{g in relevant grades} g.grader.avail_score

    # If the score isn't ready yet, or if it's not released to students, then we show the blinded scores

    @assignment = assignment
    @submissions = submissions
    @graders = @assignment.graders_ordered
    @questions = @assignment.flattened_questions
    @max_score = @graders.sum(&:avail_score)
    raw_grades = Grade.where(submission_id: @submissions.map(&:id))
    @raw_grades = raw_grades
                  .group_by(&:submission_id)
                  .map{|sid, gs| [sid, gs.map{|g| [g.grader_id, g]}.to_h]}
                  .to_h
    @missing_grades = false

    @raw_score = 0
    @grades = {graders: @graders, grades: {}}
    if @questions
      @comments = InlineComment.where(submission: @submissions, suppressed: false)
                  .where('line < ?', @questions.count)
                  .group_by(&:submission_id)
    end
    @submissions.each do |s|
      s_scores = {raw_score: 0.0, scores: []}
      b_scores = {raw_score: 0.0, scores: []}
      q_scores = []
      res = {sub: s, staff_scores: s_scores, blind_scores: b_scores, q_scores: q_scores}
      @graders.each do |c|
        g = @raw_grades.dig(s.id, c.id)
        if (c.is_a? ExamGrader)
          # q_grades = InlineComment.where(submission: s, grade: @raw_grades.dig(s.id, c.id), suppressed: false)
          #            .where('line < ?', @questions.count)
          #            .order(:line).pluck(:weight)
          q_grades = (@comments[s.id] || [])
                     .select{|comm| comm[:grade_id] == g.id}
                     .sort_by!{|comm| comm[:line]}
                     .map(&:weight)
          q_scores << q_grades
        elsif (c.is_a? QuestionsGrader)
          # q_grades = InlineComment.where(submission: s, grade: @raw_grades.dig(s.id, c.id), suppressed: false)
          #            .where('line < ?', @questions.count)
          #            .order(:line).zip(@questions)
          #            .map{|w, q| w[:weight].clamp(0, 1) * q["weight"]}
          q_grades = (@comments[s.id] || [])
                     .select{|comm| comm[:grade_id] == g.id}
                     .sort_by!{|comm| comm[:line]}.zip(@questions)
                     .map{|w, q| w[:weight].clamp(0, 1) * q["weight"]}
          q_scores << q_grades
        elsif (c.is_a? CodereviewGrader) && File.file?(g.grading_output.to_s)
          responses = YAML.load(File.read(g.grading_output))
          responses.delete("grader")
          q_grades = responses.values.flatten.zip(@questions * c.review_count).map do |r, q|
            (r["score"].to_f.clamp(0, 1) * q["weight"])
          end
          q_scores << q_grades
        end
        if g
          if g.out_of.to_f.zero?
            scaled = g.score
          else
            scaled = g.score.to_f * (c.avail_score.to_f / g.out_of.to_f)
          end
          if g.available
            s_scores[:raw_score] += scaled
            s_scores[:scores].push [scaled, c.avail_score]

            b_scores[:scores].push [scaled, c.avail_score]
            if b_scores[:raw_score] 
              b_scores[:raw_score] += scaled
            end
          else
            s_scores[:raw_score] += g.score.to_f
            s_scores[:scores].push [g.score, c.avail_score, "hidden"]
            b_scores[:raw_score] = nil
            b_scores[:scores].push "not ready"
          end
        else
          s_scores[:scores].push "Missing"
          b_scores[:scores].push "Missing"
          @missing_grades = true
        end
      end
      @grades[:grades][s.id] = res
    end
  end
end
