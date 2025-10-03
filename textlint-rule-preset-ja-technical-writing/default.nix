{pkgs, sources}:
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "textlint-rule-preset-ja-spacing";
  version = "2.4.3";

  src = pkgs.fetchFromGitHub {
    owner = "textlint-ja";
    repo = "textlint-rule-preset-ja-spacing";
    tag = "v${finalAttrs.version}";
    hash = "sha256-M27qhjIHMcKbuPAh523Pi5IB5BD0VWawh84kUyLcKvg=";
  };

  offlineCache = pkgs.fetchYarnDeps {
    yarnLock = "${finalAttrs.src}/yarn.lock";
    hash = "sha256-AfbYACqYBvfsKzhryQabXQQmera19N/UH67sR5kbihM=";
  };

  nativeBuildInputs = with pkgs: [
    nodejs
    yarnBuildHook
    yarnConfigHook
  ];

  installPhase = ''
    runHook preInstall

    yarn install \
      --force \
      --frozen-lockfile \
      --ignore-engines \
      --ignore-platform \
      --ignore-scripts \
      --no-progress \
      --non-interactive \
      --offline \
      --production=true

    mkdir -p $out/lib
    cp -r . $out/lib

    runHook postInstall
  '';
})
