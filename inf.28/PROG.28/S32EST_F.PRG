parameter pProce 

errorfis = .f.

pProce ="Estado"

set sysmenu to defa

do mpath9+'s00abre' with 'fiscal','excl'
do mpath9+'s00abre' with 't_fiscal',' '
do mpath9+'s00abre' with 'd_fiscal',' '

dele file pfbatch.tmp
dele file archfis.txt

fp = FCREATE('pfisin.txt')	

** !pfbatch /c:1 /i:cierrez.fac /o:archfis.txt
= pro_fiscal()

do rEstado

sele d_fiscal
if AT('ERROR',upper(detalle)) > 0
	wait wind  ' Error en Estado de Impresora Fiscal - Por favor Verifique  '
	errorfis = .t.
else
	wait wind " Estado OK - Digite una tecla "
endif

return
**

** -------------------- Estado FIscal ---------------------  **
procedure rEstado
comando="@ESTADO"
string="|00001|ESTADO| | "
=Enviar(comando,string)
retu 
**

*** -------------------
function pro_fiscal

sele fiscal
zap

* appe from ArchFis.txt sdf
appe from c:\estacion\pfisout.txt sdf 


** -----------  Grabo fiscal en d_fiscal y t_fiscal -------------- **
sele Fiscal
GO TOP

sele t_fiscal
set order to tag fis_In
go bott
mni_fis = t_fiscal.ni_fis+1 

appe blank
repl ni_fis  with mNi_fis ,;
     fecha   with date()  ,;
	 hora    with time()  ,;
     proceso with pProce

 
DO WHILE !EOF()
	sele d_fiscal
	appe blank
	
	repl ni_fis with mNi_fis ,;
	     detalle with fiscal.detalle 
    
    sele fiscal    
    skip 1
enddo	

return
**
*********************
* Funcion Enviar
********************
function enviar
PARAMETERS comando,String

n=fwrite(fp,comando)
n=fputs(fp,string)
return 0
**
