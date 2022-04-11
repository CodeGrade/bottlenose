{
  description = "bottlenose";

  # https://lazamar.co.uk/nix-versions/?channel=nixpkgs-unstable&package=ruby
  inputs.nixpkgs-ruby.url = "github:nixos/nixpkgs/c82b46413401efa740a0b994f52e9903a4f6dcd5";

  outputs = { self, nixpkgs, nixpkgs-ruby }: let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      config.allowUnfree = true; # for chrome
    };
    rubyPkgs = import nixpkgs-ruby {
      system = "x86_64-linux";
    };
  in {
    devShell.x86_64-linux = let
      psql_setup_file = pkgs.writeText "setup.sql" ''
        DO
        $do$
        BEGIN
          IF NOT EXISTS ( SELECT FROM pg_catalog.pg_roles WHERE rolname = 'bottlenose') THEN
            CREATE ROLE bottlenose CREATEDB LOGIN;
          END IF;
        END
        $do$
      '';
      postgres_setup = ''
        export PGDATA=$PWD/postgres_data
        export PGHOST=$PWD/postgres
        export LOG_PATH=$PWD/postgres/LOG
        export PGDATABASE=postgres
        export DATABASE_CLEANER_ALLOW_REMOTE_DATABASE_URL=true
        if [ ! -d $PGHOST ]; then
          mkdir -p $PGHOST
        fi
        if [ ! -d $PGDATA ]; then
          echo 'Initializing postgresql database...'
          initdb $PGDATA --auth=trust >/dev/null
        fi
      '';
      gem_setup = ''
        mkdir -p .nix-gems
        export GEM_HOME=$PWD/.nix-gems
        export GEM_PATH=$GEM_HOME
        export PATH=$GEM_HOME/bin:$PATH
        gem install --conservative bundler
      '';
      start_postgres = pkgs.writeShellScriptBin "start_postgres" ''
        pg_ctl start -l $LOG_PATH -o "-c listen_addresses= -c unix_socket_directories=$PGHOST"
        psql -f ${psql_setup_file} > /dev/null
      '';
      stop_postgres = pkgs.writeShellScriptBin "stop_postgres" ''
        pg_ctl -D $PGDATA stop
      '';
      chromeWrapper = pkgs.writeShellScriptBin "chrome" ''
        ${pkgs.google-chrome}/bin/google-chrome-stable $@
      '';
    in pkgs.mkShell {
      name = "bottlenose";
      buildInputs = (with pkgs; [
        git
        postgresql
        qt512.full
        bundix
        curl.dev
        pcre
        nodejs
        start_postgres
        stop_postgres
        chromedriver
        chromeWrapper
      ]) ++ (with rubyPkgs; [
        ruby_2_7
      ]);

      shellHook = ''
        ${gem_setup}
        ${postgres_setup}
      '';
    };
  };
}
