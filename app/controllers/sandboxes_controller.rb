class SandboxesController < ApplicationController
  before_filter :require_site_admin
  before_action :set_sandbox, only: [:show, :edit, :update, :destroy]

  # GET /sandboxes
  def index
    @sandboxes = Sandbox.all
  end

  # GET /sandboxes/1
  def show
  end

  # GET /sandboxes/new
  def new
    @sandbox = Sandbox.new
  end

  # GET /sandboxes/1/edit
  def edit
  end

  # POST /sandboxes
  def create
    @sandbox = Sandbox.new(sandbox_params)

    if @sandbox.save
      redirect_to @sandbox, notice: 'Sandbox was successfully created.'
    else
      render :new
    end
  end

  # PATCH/PUT /sandboxes/1
  def update
    if @sandbox.update(sandbox_params)
      redirect_to @sandbox, notice: 'Sandbox was successfully updated.'
    else
      render :edit
    end
  end

  # DELETE /sandboxes/1
  def destroy
    @sandbox.destroy
    redirect_to sandboxes_url, notice: 'Sandbox was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_sandbox
      @sandbox = Sandbox.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def sandbox_params
      params.require(:sandbox).permit(:name, :submission_id)
    end
end
