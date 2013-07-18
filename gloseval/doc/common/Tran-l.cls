%------------------------------------------------------------------------------
% Beginning of tran-l.cls
%------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{tran-l}
              [1994/10/01 v1.2 TRAN Author Class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{amsart}}
\ProcessOptions

\LoadClass{amsart}

\def\serieslogo@{\vtop to 0pt{%
  \parindent\z@ \fontsize{6}{7\p@}\selectfont
	TRANSACTIONS OF THE\newline
	AMERICAN MATHEMATICAL SOCIETY\newline
	Volume {\tiny\bfseries\currentvolume}, Number \currentissue,
	\currentmonth\ \currentyear\endgraf\vss}}

\def\ISSN{0002-9947}

\endinput
%------------------------------------------------------------------------------
% End of tran-l.cls
%------------------------------------------------------------------------------
