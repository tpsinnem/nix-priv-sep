# An example configuration.

{ config, pkgs, ... }:

let psPkgs = import ./priv-sep-pkgs; in

{
  imports = [ ./priv-sep-pkgs/priv-sep/default.nix ];

  nixpkgs.config.packageOverrides = pkgs: {
     slim = psPkgs.slim; 
  };

  users.privSepUsers = [
    { name = "alice";
      domains = [
        { name = "notes";
          commands = [
            { command = "${pkgs.fluxbox}/bin/fluxbox";
              alias = "notes"; } ];
        }
        { name = "web";
          commands = [
            { command = "${pkgs.fluxbox}/bin/fluxbox";
              alias = "web"; } ];
        }
        { name = "project-foo";
          commands = [
            { command = "${pkgs.fluxbox}/bin/fluxbox";
              alias = "foo"; } ];
        }
      ];
    }
    { name = "bob";
      domains = [
        { name = "project-foo";
          commands = [
            { command = "${pkgs.i3}/bin/i3";
              alias = "foo"; } ];
        } 
      ];
    }
 ];  

}
