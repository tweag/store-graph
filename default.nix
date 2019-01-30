# based on a file that ciruclated in the NIX mailing list.
# https://www.mail-archive.com/nix-dev@lists.science.uu.nl/msg37961.html
{ nixpkgsPath ? ./nixpkgs.nix}:
let
  pkgs = import nixpkgsPath {
    # ensure we don't get an impure config
    config = { allowUnfree = false; allowBroken = false; };
  };

  lib = pkgs.lib;

  canEval = val: (builtins.tryEval val).success;

  canEvalStrict = val: (builtins.tryEval (builtins.deepSeq val val)).success;

  # myfilter arguments
  recurseYes = true;
  recurseNo = false;
  packs = ["beamPackages" "darwinPackages" "haxePackages" "haskellPackages"
           "dotnetPackages" "emacsPackages" "emscriptenPackages"
           "javaPackages" "luaPackages" "ocamlPackages"
           "perlPackages" "phpPackages" "pythonPackages"
           "unixTools" "winePackages"];

  myfilter = platform: recurse: depth: level:
      lib.mapAttrs
        (n: v:
          let
            res = builtins.tryEval (
              (
                # for recurseForDerivations and recurseForRelease (bool)
                if builtins.typeOf v == "bool" then
                  v
                else if lib.isDerivation v && canEval v.drvPath &&
  builtins.elem platform (v.meta.platforms or []) then
                  v
                else if recurse && ((v.recurseForDerivations or false ||
  v.recurseForRelease or false) || ((builtins.typeOf v) == "set" && builtins.elem n packs && depth < 2)) then
                  myfilter platform recurse (depth+1) ''${level}.${n}'' v
                else if !canEval v then
                  {}
                else
                  {}
              )
            );
          in
            if res.success then res.value else {}
          );

  concatInputs = pkgs.lib.concatMapStrings (x: (builtins.toString x) + " ");
  getStoreInfo = platform: recurse: depth: level:
    lib.mapAttrs
    (n: v: 
        let
          res=builtins.tryEval (
            (
             if recurse && (builtins.typeOf v) == "set" && builtins.elem n packs && depth < 2 then
                getStoreInfo platform recurse (depth + 1) ''${level}.${n}'' v
             else
               ''${n}, '' +
               ''${level}, '' +
               ''${builtins.toString (builtins.typeOf v)}, '' +
               ''${if v ? name then v.name else ""}, '' +
               ''${if v ? drvPath then (builtins.toString v.drvPath) else ""}, '' +
               ''${if v ? buildInputs then (concatInputs v.buildInputs) else ""}''
               )
               );
        in
          if res.success then res.value else ''${n}, ${level}, , , ,''
          );

  nestedPkgsToString = pkgs.lib.concatMapStrings
  (x: (if ((builtins.typeOf x) == "set") then nestedPkgsToString (builtins.attrValues x) else if x==null then "" else x + "\n"));


in rec
{
  inherit pkgs;

  # This shouldn't stop due to eval errors, but expect plenty of build
  # failures. Better run with "--keep-going" if you want to build as much as
  # possible.
  filtered = myfilter "x86_64-linux" recurseYes 1 "pkgs" pkgs;
  storeInfo = getStoreInfo "x86_64-linux" recurseYes 1 "pkgs" pkgs;
  storeFile = nestedPkgsToString (pkgs.lib.attrValues storeInfo);
}
