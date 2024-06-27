class TermsController < ApplicationController
  before_action :require_current_user
  before_action -> { require_admin_or_prof(root_path) }
  def index
    @terms = Term.all_sorted
  end

  def show
    find_term
  end

  def new
    @term = Term.new(year: Date.current.year)
    case Date.current.month # NOTE: not all months pre-populate a guess for semester
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
    find_term
  end

  def create
    @term = Term.new(term_params)

    if @term.save
      redirect_to term_path(@term), notice: 'Term was successfully created.'
    else
      render action: "new", status: 400
    end
  end

  def update
    find_term

    if @term.update(term_params)
      redirect_to term_path(@term), notice: 'Term was successfully updated.'
    else
      render action: "edit", status: 400
    end
  end

  def destroy
    find_term
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

  def find_term
    @term = Term.find_by(id: params[:id])
    if @term.nil?
      redirect_back fallback_location: terms_path, alert: "No such term"
      return
    end
  end
end
