{
  pkgs,
  sources,
}:
pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "textlint";
  version = sources.textlint.version;
  src = sources.textlint.src;
  nativeBuildInputs = with pkgs; [
    makeWrapper
    nodejs
    pnpm.configHook
  ];
  pnpmDeps = pkgs.pnpm.fetchDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 2;
    hash = "sha256-XLu0RY/xeyZYAuq0wh7JL7afQsVmjQm/i8rch4kTa+k=";
  };
  # refernece: https://github.com/NixOS/nixpkgs/blob/24ae1fef56947f3de16401f00458f2e1e0fb177d/pkgs/by-name/te/textlint/package.nix#L69
  buildPhase = ''
    runHook preBuild

    pnpm --filter textlint... build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/{bin,lib/node_modules}

    rm -r node_modules
    rm -r packages/textlint/node_modules
    rm -r packages/@textlint/**/node_modules
    pnpm install --offline --ignore-scripts --frozen-lockfile --prod --filter textlint...

    cp -r packages/{textlint,@textlint} $out/lib/node_modules
    cp -r node_modules/.pnpm $out/lib/node_modules

    makeWrapper "${pkgs.lib.getExe pkgs.nodejs}" "$out/bin/textlint" \
      --add-flags "$out/lib/node_modules/textlint/bin/textlint.js"

    # Remove dangling symlinks to packages we didn't copy to $out
    find $out/lib/node_modules/.pnpm -type l -exec test ! -e {} \; -delete

    # Remove test directories recursively
    find $out/lib/node_modules -type d -name "test" -exec rm -rf {} +

    runHook postInstall
  '';
  # installPhase = ''
	# mkdir -p $out/bin
	# cp -r examples/cli/node_modules/textlint $out/bin
	# '';
})
