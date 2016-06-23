unit TransSkelLib;
interface


{$define TRANSSKELLIBINTF}
{$setc TRANSSKELLIBINTF := TRUE}
{$setc TRANSSKELLIB := FALSE}

{$i TransSkel5.pas}
{$i TransEdit5.pas}
{$i TransDisplay5.pas}
{$i QDCG.pas}
{$i QDCGPictUnit.pas}

implementation

{$define TRANSSKELLIB}
{$setc TRANSSKELLIBINTF := FALSE}
{$setc TRANSSKELLIB := TRUE}


end.
