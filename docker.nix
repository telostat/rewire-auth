{ tag ? "latest"
, ...
}:

let
  ## Import this codebase's Nix helper set:
  nix = import ./nix { };

  ## Get packages:
  pkgs = nix.pkgs;
in
pkgs.dockerTools.buildImage {
  name = "telostat/rewire-auth";
  tag = tag;
  created = "now";

  contents = [
    pkgs.bash
    pkgs.cacert
    pkgs.coreutils
  ];

  runAsRoot = ''
    #!${pkgs.runtimeShell}
    ${pkgs.dockerTools.shadowSetup}
    mkdir /tmp
    chmod 777 /tmp
    groupadd -r users
    useradd -r -g users patron
  '';

  config = {
    Entrypoint = [ "${pkgs.haskell.lib.justStaticExecutables nix.thisPackageInstallable}/bin/rewire-auth" ];
    Cmd = [ "serve" ];
    User = "patron";
  };
}
