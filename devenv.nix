{ pkgs, ... }:

{
  # https://devenv.sh/basics/

  # https://devenv.sh/packages/
  packages = [ 
    pkgs.git 
    pkgs.just
    pkgs.elmPackages.lamdera
  ];

  # https://devenv.sh/languages/
  languages.elm.enable = true;
  languages.javascript.enable = true;
  languages.javascript.npm.install.enable = true;

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
