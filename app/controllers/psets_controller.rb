class PsetsController < ApplicationController
  before_action :find_course
  before_action :require_registered_user
  before_action :set_pset, only: [:show, :edit, :update, :destroy]
  before_action :require_admin_or_prof, only: [:edit, :update, :new, :create, :destroy]
  before_action :require_admin_or_staff, only: [:tarball]

  # GET /psets
  def index
    @psets = Pset.all
  end

  # GET /psets/1
  def show
    if current_user_site_admin? || current_user_staff_for?(@course)
      staff_show
      return
    end

    @subs = current_user.submissions_for(@pset).includes(:user).order(created_at: :desc).to_a
    @gradesheet = Gradesheet.new(@pset, @subs)
  end

  def staff_show

    render 'staff_show'
  end

  # GET /psets/new
  def new
    @pset = Pset.new
  end

  # GET /psets/1/edit
  def edit
  end

  # POST /psets
  def create
    @pset = Pset.new(pset_params)
    @pset.course_id = @course.id
    @pset.blame_id  = current_user.id

    if @pset.save
      redirect_to @pset, notice: 'Pset was successfully created.'
    else
      render :new
    end
  end

  # PATCH/PUT /psets/1
  def update
    if @pset.update(pset_params)
      redirect_to @pset, notice: 'Pset was successfully updated.'
    else
      render :edit
    end
  end

  # DELETE /psets/1
  def destroy
    @pset.destroy
    redirect_to psets_url, notice: 'Pset was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_pset
      @pset = Pset.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def pset_params
      params.fetch(:pset).permit(:name, :assignment, :due_date, :available,
                                 :points_available, :hide_grading, :blame_id,
                                 :assignment_file, :course_id, :team_subs,
                                 :request_time_taken, :lateness_config_id, :removefile,
                                 lateness_config_attributes: [
                                   :type, :percent_off, :frequency,
                                   :max_penalty, :days_per_assignment,
                                   :_destroy
                                 ],
                                 graders_attributes: [
                                   :avail_score, :upload_file, :params,
                                   :type, :id, :_destroy
                                 ]
                                )
    end
end
