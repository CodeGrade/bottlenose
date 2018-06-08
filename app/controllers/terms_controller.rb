class TermsController < ApplicationController
  before_action :require_current_user
  def index
    @terms = Term.all_sorted
  end

  def show
    @term = Term.find(params[:id])
  end

  def new
    @term = Term.new(year: Date.today.year)
    case Date.today.month # NOTE: not all months pre-populate a guess for semester
    when 8, 9
      @term.semester = Term.semesters[:fall]
    when 12, 1
      @term.semester = Term.semesters[:spring]
    when 4, 5
      @term.semester = Term.semesters[:summer_1]
    when 6, 7
      @term.semester = Term.semesters[:summer_2]
    end
  end

  def edit
    @term = Term.find(params[:id])
  end

  def create
    unless current_user_site_admin?
      redirect_to root_path, alert: "Must be an admin."
      return
    end

    @term = Term.new(term_params)

    if @term.save
      redirect_to term_path(@term), notice: 'Term was successfully created.'
    else
      render action: "new"
    end
  end

  def update
    unless current_user_site_admin?
      redirect_to root_path, alert: "Must be an admin."
      return
    end

    @term = Term.find(params[:id])

    if @term.update_attributes(term_params)
      redirect_to term_path(@term), notice: 'Term was successfully updated.'
    else
      render action: "edit"
    end
  end

  def destroy
    unless current_user_site_admin?
      redirect_to root_path, alert: "Must be an admin."
      return
    end

    @term = Term.find(params[:id])
    @term.destroy

    redirect_to terms_url
  end

  private

  def setup_breadcrumbs
    add_root_breadcrumb
    add_breadcrumb "Courses", courses_path
    add_breadcrumb "Terms"
  end

  def term_params
    params.require(:term).permit(:year, :semester, :archived, :id)
  end
end
