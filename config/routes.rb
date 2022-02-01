Bottlenose::Application.routes.draw do
  use_doorkeeper do
    # it accepts :authorizations, :tokens, :token_info, :applications and :authorized_applications
    controllers applications: 'oauth/applications'
    controllers authorizations: 'oauth/authorizations'
  end

  namespace :api do
    get '/me' => 'users#me'
    resources :courses, only: [:index] do
      resources :registrations, only: [:index]
      resources :assignments, only: [] do
        collection do
          post :create_or_update
        end
      end
    end
  end

  resources :sandboxes

  # Using devise for user auth.
  devise_for :users, :skip => [:registrations, :passwords, :sessions]
  devise_scope :user do
    get "/login" => "devise/sessions#new", :as => :new_user_session
    post "/login" => "devise/sessions#create", :as => :user_session
    delete "/logout" => "devise/sessions#destroy", :as => :destroy_user_session
  end

  root to: "main#home"
  get "about" => "main#about"

  resource :settings, only: [:edit, :update]

  resources :users, except: [:destroy] do
    collection do
      post :stop_impersonating
      get :lookup, to: 'users#lookup'
    end
    member do
      post :impersonate
    end
  end

  get 'files/*path', to: 'files#upload', constraints: {path: /.*/}
  
  resources :terms

  resources :courses, except: [:destroy] do
    resources :grades, only: [] do
      collection do
        get 'stats' => 'grader_allocations#stats', as: 'stats'
      end
    end
    resources :grader_allocations, only: [] do
      member do
        patch :abandon
        patch :delete
      end
    end

    resources :registrations, except: [:show, :edit, :update] do
      collection do
        get :bulk_edit
        post :bulk_update
        post :bulk_enter
      end
    end
    resources :reg_requests, only: [:new, :create] do
      member do
        delete :accept
        delete :reject
      end
      collection do
        delete :accept_all
        delete :reject_all
      end
    end
    resources :assignments do
      collection do
        get 'weights' => 'assignments#edit_weights'
        post 'weights' => 'assignments#update_weights'
      end
      member do
        get 'audit_access' => 'assignments#audit_access'
        post 'create_missing_grades' => 'assignments#recreate_grades'
      end
      resources :grader_allocations, only: [] do
        collection do
          get ':grader_id' => 'grader_allocations#index', as: ''
          get ':grader_id/edit' => 'grader_allocations#edit', as: 'edit'
          patch ':grader_id/edit' => 'grader_allocations#patch', as: 'patch'
          patch ':grader_id/update' => 'grader_allocations#update', as: 'update'
          delete :abandon_all
          delete :delete_all
        end
      end
      resources :graders, only: [] do
        member do
          get 'all_grades' => 'grades#tarball'
          get 'bulk' => 'grades#bulk_edit'
          post 'bulk' => 'grades#bulk_update'
          get 'bulk_curve' => 'grades#bulk_edit_curve'
          patch 'bulk_curve' => 'grades#bulk_update_curve'
        end
      end
      resources :matchings, only: [] do
        collection do
          get 'edit' => 'matching_allocations#edit', as: 'edit'
          patch 'edit' => 'matching_allocations#patch', as: 'patch'
          patch 'update' => 'matching_allocations#update', as: 'update'
          patch 'bulk_enter' => 'matching_allocations#bulk_enter', as: 'bulk_enter'
          delete 'delete_all' => 'matching_allocations#delete_all', as: 'delete_all'
        end
        member do
          delete 'delete' => 'matching_allocations#delete', as: 'delete'
        end
      end
      resources :extensions, only: [] do
        collection do
          get 'edit' => 'individual_extensions#edit', as: 'edit'
          patch 'edit' => 'individual_extensions#patch', as: 'patch'
          patch 'update' => 'individual_extensions#update', as: 'update'
          delete 'delete' => 'individual_extensions#delete', as: 'delete'
          delete 'delete_all' => 'individual_extensions#delete_all', as: 'delete_all'
        end
      end
      resources :submissions, except: [:edit, :update, :destroy] do
        collection do
          post 'rerun/:grader_id', to: 'submissions#rerun_grader', as: 'rerun_grader'
        end
        member do
          get :details
          post :team_use, to: 'submissions#use_for_everyone', as: 'team_use'
          post 'user_use/:user_id', to: 'submissions#use_for_student', as: 'user_use'
          # user/:user_id 
          patch :publish, to: 'submissions#publish', as: 'publish'
          patch :rescind_lateness, to: 'submissions#rescind_lateness', as: 'rescind_lateness'
          post 'recreate/:grader_id', to: 'submissions#recreate_grade', as: 'recreate_grade'
          get 'plagiarism', to: 'submissions#edit_plagiarism', as: 'edit_plagiarism'
          patch 'plagiarism', to: 'submissions#update_plagiarism', as: 'update_plagiarism'
          patch 'split', to: 'submissions#split_submission', as: 'split'          
        end
        resources :reviews, only: [:show] 
        resources :grades, only: [:show, :edit, :update] do
          member do
            post :regrade
            get :details, defaults: {format: 'text'}
          end
        end
      end

      resources :submission_enabled_toggles, only: [] do
        patch 'update' => 'assignments#update_section_toggles', as: 'update'
      end
    end
    resources :teamsets, only: [:index, :edit, :update] do
      collection do
        get :investigate
        get :export
      end
      resources :team_requests, only: [:index, :create, :update, :destroy]
      member do
        patch :clone
        patch :randomize
        patch :bulk_enter
        patch :dissolve_all
        delete :accept_all_requests
        delete :reject_all_requests
        delete :accept_request
        delete :reject_request
      end
      resources :teams, only: [:show] do
        member do
          patch :dissolve
        end
      end
    end
    member do
      get :public
      get :gradesheet
      get :facebook
      delete :withdraw
    end
  end

  get   'courses/:course_id/assignments/:id/user/:user_id' => 'assignments#show_user', as: 'course_assignment_user'
  get   'courses/:course_id/assignments/:id/summarysheet' => 'assignments#summarysheet', as: 'course_assignment_summarysheet'
  get   'courses/:course_id/assignments/:id/tarball' => 'assignments#tarball', as: 'course_assignment_tarball'
  patch 'courses/:course_id/assignments/:id/publish' => 'assignments#publish', as: 'course_assignment_publish'
  get   'status' => 'main#status', as: 'server_status'
  patch 'clear_queue' => 'main#clear_queue', as: 'clear_queue'

  match "/500", :to => "errors#internal_server_error", :via => :all
  get "*any", via: :all, to: "errors#not_found"
end
