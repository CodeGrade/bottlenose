class TeamsController < CoursesController
  before_action :require_admin_or_staff, only: [:new, :create, :dissolve, :dissolve_all, :randomize]
  # GET /staff/courses/:course_id/teams
  def index
    @course = Course.find(params[:course_id])
    if current_user_site_admin? || current_user_staff_for?(@course)
      @active_teams = @course.teams.select(&:active?)
      @inactive_teams = @course.teams.reject(&:active?)
    else
      @active_teams = current_user.teams.select(&:active?)
      @inactive_teams = current_user.teams.reject(&:active?)
    end
  end

  # GET /staff/course/:course_id/teams/new
  def new
    @course = Course.find(params[:course_id])
    @team = Team.new
    @team.course_id = @course.id
    @teams = @course.teams.select(&:active?)
    @others = students_without_active_team
  end

  # POST /staff/course/:course_id/teams
  def create
    @course = Course.find(params[:course_id])
    @team = Team.new(team_params)

    users = params["users"] || []
    @team.users = users.map { |user_id| User.find(user_id.to_i) }

    if @team.save
      redirect_to new_course_team_path(@course),
        notice: 'Team was successfully created.'
    else
      @teams = @course.teams.select(&:active?)
      @others = students_without_active_team
      render :new
    end
  end

  # GET /courses/:course_id/teams/:id
  def show
    @course = Course.find(params[:course_id])
    @team = Team.find(params[:id])

    if !(current_user_site_admin? or current_user_staff_for?(@course)) and @team.users.exclude?(current_user)
      redirect_to(root_path, alert: "You are not a member of that team.")
    end
  end

  def dissolve
    @team = Team.find(params[:id])
    @team.dissolve(DateTime.current)
    redirect_to back_or_else(course_teams_path(@course))
  end


  def dissolve_all
    teams = Team.where(course: @course, end_date: nil)
    teams.each do |t| t.dissolve(DateTime.current) end
    redirect_to back_or_else(course_teams_path(@course)), notice: "#{plural(teams.count, 'team')} dissolved"
  end

  def randomize
    count = 0
    students_without_active_team.to_a.shuffle.each_slice(params[:random][:size].to_i).each do |t|
      @team = Team.new(course: @course, start_date: params[:random][:start_date], end_date: params[:random][:end_date])
      @team.users = t

      if @team.save
        count += 1
      end
    end
    redirect_to back_or_else(course_teams_path(@course)), notice: "#{plural(count, 'random team')} created"
  end

  private

  def students_without_active_team
    # TODO: Optimize.
    @course.students_with_drop_info.where("registrations.dropped_date": nil).reject do |student|
      student.teams.where(course: @course).any? { |t| t.active? }
    end
  end

  def team_params
    params.require(:team).permit(:course_id, :start_date, :end_date)
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to course_teams_path, alert: "Must be an admin or staff."
      return
    end
  end    
end
