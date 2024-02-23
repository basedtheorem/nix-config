let
  genPrelude =
    {
      name,
      tagLine,
      commentLines,
    }:
    let
      comments = builtins.concatStringsSep "\n" (
        builtins.map (l: if l == "" then "" else ";; ${l}") commentLines
      );
    in
    ''
      ;;; ${name} --- ${tagLine}

      ;;; Commentary:

      ${comments}

      ;;; Code:
    '';

  genPostlude = name: ''
    (provide '${name})
    ;;; ${name}.el ends here
  '';

  genRequires =
    list:
    let
      sorted = builtins.sort (l: r: l < r) list;
      required = builtins.map (r: "(require '${r})") sorted;
    in
    builtins.concatStringsSep "\n" required;
in
name: tagLine: commentLines: requireList: code: ''
  ${genPrelude { inherit name tagLine commentLines; }}

  ${genRequires requireList}

  ${code}

  ${genPostlude name}
''
