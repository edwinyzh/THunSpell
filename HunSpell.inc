// DEBUG or RELEASE version
{x$define DEBUG}

{$define WIN32}
// Is it an UNICODE program
{x$define UNICODE}

{$ifdef UNICODE}
  {$define USE_TNT}
{$endif}

// Add useful defines
{$i jedi.inc}

{$ifdef DEBUG}
  {$o+}
  {$d+,r+,q+,c+}
{$else}
  {$o+}
  {$d-,r-,q-,c-}
{$endif}

{$j-}
