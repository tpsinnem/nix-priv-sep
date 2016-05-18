{ lib, pkgs }:

with lib;

let xephyrLauncher = (import ../xephyr-launcher {}); in

rec {

addPrivSepUser =       
  users: psu:
    users // { 

      "${psu.name}" = {

        home            = "/home/" + psu.name;
        # uid             = psu.uid;
        group           = psu.name;
        extraGroups     = ["users"];
        createHome      = false; /* Home will be created for and owned by psu.name + "-admin"  */
        useDefaultShell = true;
        isSystemUser    = false; };

      "${psu.name}-admin" = {

        home            = "/home/" + psu.name;
        # uid             = psu.adminUid;
        group           = psu.name;
        extraGroups     = ["users"];
        createHome      = true;
        useDefaultShell = true;
        isSystemUser    = false; };

    } // foldl (addDomain psu) {} psu.domains;

addDomain =
  psu: users: domain:
    users // {

     "${psu.name}-${domain.name}" = {

       home            = "/priv-sep/" + psu.name + "/" + domain.name + "/work";
       # uid             = domain.uid;
       group           = psu.name + "-" + domain.name;
       createHome      = true;
       useDefaultShell = true;
       isSystemUser    = false; };

     "${psu.name}-${domain.name}-xiso" = {

       home         = "/home/" + psu.name;
       # uid          = domain.xIsoUid;
       group        = psu.name + "-" + domain.name;
       extraGroups  = [psu.name];
       createHome   = false;
       isSystemUser = false; }; };


addUserGroupAndDomainGroups =
  groups: psu:
    # groups // { "${psu.name}".gid = psu.gid; } //
    groups // { "${psu.name}" = {}; } //
    foldl
      ( groupss: domain:
        # groupss // { "${psu.name}-${domain.name}".gid = domain.gid; } )
        groupss // { "${psu.name}-${domain.name}" = {}; } )
        {}
        psu.domains;


addSudoCfg =
  sudocfg: psu:
    sudocfg + "\n" + "\n" +
    foldl (addDomainSudoCfg psu) "" psu.domains;

addDomainSudoCfg =
  psu: sudocfg: domain:

    sudocfg + "\n" + "\n" +

    "${psu.name} ALL = (${psu.name}-${domain.name}-xiso) NOPASSWD: " +
      "${xephyrLauncher}/bin/xephyr-server-launcher " +
        "/priv-sep/${psu.name}/${domain.name}/.xephyr-status " +
        "${pkgs.xorg.xorgserver}/bin/Xephyr " +
        "${pkgs.xorg.xauth}/bin/xauth " +
        "${pkgs.utillinux}/bin/mcookie" +

    foldl (addCommandSudoCfg psu domain) "" domain.commands;

addCommandSudoCfg =
  psu: domain: sudocfg: commandAndAlias:

    sudocfg + "\n" +

    "${psu.name} ALL = (${psu.name}-${domain.name}) NOPASSWD: " +
      "${xephyrLauncher}/bin/xephyr-client-launcher " +
        "/priv-sep/${psu.name}/${domain.name}/.xephyr-status " +
        commandAndAlias.command;


addDirScripts =
  script: psu:
    script + ''
    chmod g+rx /home/${psu.name}
    chmod go+rx /priv-sep/${psu.name}
    '' +
    foldl (addDomainDirScripts psu) "" psu.domains;

addDomainDirScripts = /* TODO THINK is go+rx for domain right? */
  psu: script: domain:
    script + ''

    chmod go+rx /priv-sep/${psu.name}/${domain.name}

    mkdir -p /priv-sep/${psu.name}/${domain.name}/in
    chown ${psu.name}.${psu.name}-${domain.name} /priv-sep/${psu.name}/${domain.name}/in
    chmod o-rx /priv-sep/${psu.name}/${domain.name}/in

    mkdir -p /priv-sep/${psu.name}/${domain.name}/out
    chown ${psu.name}-${domain.name}.${psu.name} /priv-sep/${psu.name}/${domain.name}/out
    chmod o-rx /priv-sep/${psu.name}/${domain.name}/out

    mkdir -p /priv-sep/${psu.name}/${domain.name}/.xephyr-status
    chown ${psu.name}-${domain.name}.${psu.name}-${domain.name} /priv-sep/${psu.name}/${domain.name}/.xephyr-status
    chmod o-rx /priv-sep/${psu.name}/${domain.name}/.xephyr-status
    chmod g+rwx /priv-sep/${psu.name}/${domain.name}/.xephyr-status
    '';


addSudoCommands =
  script: psu:
    script + "\n" +
    foldl (addDomainSudoCommands psu) "" psu.domains;

addDomainSudoCommands =
  psu: script: domain:
    script + "\n" +
    foldl (addSudoCommandSet psu domain) "" domain.commands;

addSudoCommandSet =
  psu: domain: script: commandAndAlias:

    let commandFile = 
      pkgs.writeScript "priv-sep-sudo-command" (
        "#!/bin/sh" + "\n" +
          "/var/setuid-wrappers/sudo -u ${psu.name}-${domain.name}-xiso " +
            "${xephyrLauncher}/bin/xephyr-server-launcher " +
              "/priv-sep/${psu.name}/${domain.name}/.xephyr-status " +
              "${pkgs.xorg.xorgserver}/bin/Xephyr " +
              "${pkgs.xorg.xauth}/bin/xauth " +
              "${pkgs.utillinux}/bin/mcookie" +
              "\n" +
          "/var/setuid-wrappers/sudo -u ${psu.name}-${domain.name} " +
            "${xephyrLauncher}/bin/xephyr-client-launcher " +
              "/priv-sep/${psu.name}/${domain.name}/.xephyr-status " +
              commandAndAlias.command ); 
      in

    script + ''
    mkdir -p /home/${psu.name}/bin
    cp ${commandFile} /home/${psu.name}/bin/${commandAndAlias.alias}
    '';
}
