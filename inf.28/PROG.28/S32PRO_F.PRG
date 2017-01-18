parameter pProce 

* set sysmenu to defa

do mpath9+'s00abre' with 'fiscal','exclu'
do mpath9+'s00abre' with 't_fiscal',' '
do mpath9+'s00abre' with 'd_fiscal',' '

set defa to c:\estacion

dele file pfbatch.tmp
dele file c:\estacion\pfisout.txt

do case
	case pProce = "ESTADO"
         wop = 1
         do confirma with " Genera Control de Estado: " in mpath1+'w00wfun'
         if wop <> 1
            retu 
         endi
    	 !pfbatch /c:2 /i:estado.fac /o:Pfisout.txt

	case pProce = "SINCRO"
         wop = 1
         do confirma with " Genera Sincronización: " in mpath1+'w00wfun'
         if wop <> 1
            retu 
         endi
    	 !pfbatch /c:2 /i:SINCRO.fac /o:Pfisout.txt

	case pProce = "CIERREX"
         wop = 1
         do confirma with " Genera Cierre X: " in mpath1+'w00wfun'
         if wop <> 1
            retu 
         endi
	     !pfbatch /c:2 /i:cierreX.fac /o:Pfisout.txt

	case pProce = "CIERREZ"

         wop = 1
         do confirma with " Genera Cierre Z: " in mpath1+'w00wfun'
         if wop <> 1
            retu 
         endi
    	 !pfbatch /c:2 /i:cierrez.fac /o:Pfisout.txt
         wait ' Oprima una tecla Cuando termine de imprimir !!! ... ' wind 
    
    otherw 
	     wait ' Este Proceso no está Disponible !!! .... '
    	 retu 
endcase

= pro_fiscal()

sele 0
use c:\estacion\estacion alias estacion
* set defa to allt(estacion.bases) 
set defa to allt(estacion.fuentes)
return


** ---------------------------------------------------------------------
**  ------- Grabo fiscal (Pfisout.txt) en d_fiscal y t_fiscal ------- **
** ---------------------------------------------------------------------
function pro_fiscal

sele fiscal
zap

** ------------ si no esta el archivo pfisout.txt se cuelga --------------
* if not file("c:\estacion\pfisout.txt")

if not file("c:\estacion\pfisout.txt")
	wait wind "No se encuentra archivo pfisout.txt"
	return
endif

* appe from c:\estacion\pfisout.txt sdf 
appe from pfisout.txt sdf 

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

if  pProce = "ESTADO"
	do rEstado             &&& mensajes si hay error en estado
endif

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

** ---------------  Verifica si hay error / Estado     ----------------------
procedure rEstado
sele fiscal

GO TOP
if reccount()=0
	wait wind  ' Verifique que la Impresora Fiscal esté prendida '
	errorfis = .f.
endif

go top
do while !eof()
	if AT('ERROR',upper(detalle)) > 0
		errorfis = .t.
	endif
	skip 1
enddo

if errorfis
		wait wind  ' Error en Estado de Impresora Fiscal - Por favor Verifique  '
		errorfis = .f.
	else
		wait wind " Estado OK - Digite una tecla "
endif
return

