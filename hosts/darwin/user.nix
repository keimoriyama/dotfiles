{...}: let
  username = "kei";
  homeDirectory = "/Users/${username}";
  userShell = builtins.toPath "/etc/profiles/per-user/${username}/bin/fish";
in {
  environment.shells = [userShell];
  environment.etc."shells".knownSha256Hashes = [
    "9d5aa72f807091b481820d12e693093293ba33c73854909ad7b0fb192c2db193"
    "135896d22e1bb2cc94d76895b06ea185ec470551648ce841eb7adea623026970"
  ];

  programs.fish.enable = true;

  users.users.${username} = {
    name = username;
    home = homeDirectory;
    shell = userShell;
  };
}
