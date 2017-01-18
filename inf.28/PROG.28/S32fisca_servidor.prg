procedure rimprefis

set sysmenu to defa

store spac(10) to mleye1, mleye2, mleye3

ncuit  = ' ' 
doccli = ' '
mdni = '0'
errorfis = .f. 

*- do s22confir        &&&& llamo tipo de pago  

if mContado  <> 0  and mTot_Fin = 0
   *- mleye1 = 'Contado $'+ alltrim(str(mbruto,10,2))

   mleye1 = 'Cond.Vta:Contado'
   store ' ' to mleye2, mleye3

   Else  &&& mTot_Fin

  	mleye1 = 'Crédito Personal: $' + allt(str(mTot_Fin,10,2))

    if  mContado  <> 0
        mleye2 = 'Entrega: $'+ allt(str(mContado,10,2))
    endi 

endi 

    do HaceFormu in mpath13+'s32fisca'

    if errorfis
	   wait wind ' Verifique Impresora Fiscal '
	   retu
    endif

****************** ultimo **************
retu 

*
** -------------------------------------------------------------
** ------------- Venta confirmada  -------------------
*  store spac(10) to mleye1, mleye2, mleye3
*  do s22confir        &&&& llamo tipo de pago  

procedure HaceFormu
mtasaiva  = 0
*- mtasaiva  = 21
mdife     = 0
mimpoItem = 0
mTotManu  = 0

*if file("fisent")
*   FP = fopen("fisent.pfb",2)
*   else
*   fp = FCREATE('fisent.pfb')	
*endif

set defa to c:\estacion 

dele file \estacion\fisent.pfb
*- fp = FCREATE('fisent.pfb')	

fp = FCREATE('pfisin.txt')	

* ----  demo --------
do rPONEENCABEZADO
do rFACTABRE
do rFACTITEM
*- do rFACTPAGO
do rFACTCIERRA

=fclose(fp)

set defa to c:\estacion 

dele file c:\estacion\pfisout.txt 
dele file c:\estacion\pfbatch.tmp

!/n c:\estacion\pfbatch /c:1 

=cierre()

*sele 0
*use c:\estacion\estacion alias estacion

if used("estacion")
   select estacion 
else
   select 0
   use (locfile('c:\estacion\estacion.dbf',"DBF","Where is estacion?"));
      again alias estacion  
endif

* set defa to allt(estacion.bases) 

set defa to allt(estacion.fuentes) 

return
*
**************************************************************************
procedure rPONEENCABEZADO
comando="@PONEENCABEZADO"

if !empty(mleye1)
	string="||5|"+ mleye1    
	=Enviar(comando,string)
endif

*if empty(mleye2)
*	mleye2=''
*endif

*if !empty(mleye2)
*	string="||6|" + mleye2
*	=Enviar(comando,string)
*endif

*if empty(mleye3)
*	mleye3=''
*endif
*if !empty(mleye3)
*	string="||7|" + mleye3
*	=Enviar(comando,string)
*endif
retu 


** -----------------------------------------------------------
procedure rFACTABRE
** Abre la Factura o Tique-Factura

  do case
     case asitiva   = 1 &&&  case rcliente.codiva='RI'
      ivacli="I"
      DocCli="CUIT"
     case asitiva   = 2  &&& rcliente.codiva='RN'
      ivacli="R"
      DocCli="CUIT"
     case asitiva   = 3  &&& rcliente.codiva='CF'
      ivacli="F"
      DocCli="DNI"
    case asitiva    = 4  &&& rcliente.codiva='EX'
      ivacli="E"
      DocCli="CUIT"      
    case asitiva    = 5  &&& rcliente.codiva='MO'    
      ivacli="M"
      DocCli="CUIT"      
    case asitiva    = 6  &&& rcliente.codiva='NR'
      ivacli="N"   
      DocCli="CUIT"       
  endcase

  if asitiva   <> 3

     do while .t.

        pos=at("-",ncuit)
        if pos > 0
           aux=substr(ncuit,1,pos-1)+substr(ncuit,pos+1)
           store aux to ncuit
           else
          exit  
       endif
     enddo    
     ncuit=mcuit

     xcuit = ncuit 

     ncuit= left(xcuit,2)+substr(xcuit,4,8)+right(xcuit,1)
    Else 
     ncuit=mdni 
endi 

mfantasia= ''      &&&& alt(127) 
mmnomcli = allt(left(aNcli1,20))
mmdomici = allt(left(mdomici,20))

if empty(mdomici) 
    mmdomici = '' 
endi 

** -----------------------------------------------------------
comando="@FACTABRE"

string="|00001|F|F|"+mtipcom+"|1|P|12|I|"+ivacli+"|"+mmnomcli+"|"+;
        mfantasia +"|"+DocCli+"|"+ncuit+"|N|"+mmdomici+"|Formosa|Formosa|S.R.|"

=Enviar(comando,string)
          *-T (Tiquet fiscal)
          *-C (Formulario Continuo)
          *-P (Dibuja la impresora), F(Pre.Impreso), A(Autoimpreso) 

* ------  Anterior  -------- 
*-string="|00001|F|F|"+mtipcom+"|1|F|12|I|"+ivacli+"|"+mmnomcli+"|"+;
*        mfantasia +"|"+DocCli+"|"+ncuit+"|N|"+mmdomici+"|Formosa|Formosa|Sin Remito||C"

retu 
**
** --------------------------------------------------------------
procedure rFACTITEM
** Imprime Linea en Factura o Tique-Factura
*-sele tmpegr 

sele egretemp 
go top 

mimpoItem = 0
mNro_Item = 0

*- mtotmanu = totmanu

mtotmanu = mContado   &&& ver con la flaca 

do whil !eof() 

   mcodart = egretemp.codart 

   *if egretemp.tasaiva = 1.21 
   *   mtasaiva =  '2100'
   *endi 
   *if egretemp.tasaiva = 1.105 
   *   mtasaiva =  '1050'
   *endi 
   *if egretemp.tasaiva = 0 
   *   mtasaiva =  '0000'
   *endi 
   
   xtasa = (egretemp.tasaiva-1) *10000
   mtasaiva = righ('0000'+allt(str(xtasa,4)),4)
   xtasa   = ((egretemp.tasaiva-1) *10000)/2
   vporc2  = righ('0000'+allt(str(xtasa,4)),4)

   sele articulo 
   seek mcodart

   if !found()
       sele egretemp 
       skip 1 
       loop 
   endi

   mdescri = allt(articulo.descriin)  &&& 20/12/04  descriin
   mnomart = allt(articulo.nomart)

 *  if len(mnomart) < 3520
    mnomart = mnomart+' '+mdescri 
  * endi 

   nprod =  left(allt(codart)+'-'+mnomart,35)   &&& left(articulo.nomart,20)
   sele egretemp 

    mdeci = righ(str(cantidad-int(cantidad),5,3),3)
    cantunid= righ('00000'+ allt( str(int(cantidad),5)),5) +;
    righ('000'+ allt(mdeci),3)

    *- mimpoItem = mimpoItem + (preunita * cantidad)   
    mimpoItem = mimpoItem + (prevenfo * cantidad)   &&& 20/12/04

     if mtipcom ="A"
        *-elprecio=preunita - ((preunita * mtasaiva)/100)
        *- elprecio=preunita / 1.21        // 20/12/04
        elprecio= prevenfo
     else
        *- elprecio = preunita      &&&&&  +((preunita * mtasaiva )/100)
        elprecio = prevenfoci       &&&&&      // 20/12/04
     endif

    mdeci = righ(str(elprecio-int(elprecio),4,2),2)
    precio = righ('0000000'+ allt( str(int(elprecio),7)),7) +;
        righ('00'+ allt(mdeci),2)

    *- vporc1=str(mtasaiva,2,0)+"00"

     vporc1=mtasaiva
*-     vporc2="1050"

     * - - - - 

     mNro_Item = mNro_Item + 1  
     xNro_Item = righ('00000'+ allt(str(mNro_Item,5)),5)
     comando="@FACTITEM"

     if mtipcom = 'A'
        * xtasa   = ((1.21-1) *10000)/2
        * vporc2  = righ('0000'+allt(str(xtasa,4)),4)
        *-vporc2 = righ('0000'+allt(mtasaiva/2*100,4)),4)

        string="|"+xNro_Item+"|"+nprod+"|"+cantunid+"|"+precio+"|"+vporc1+;
            "|M|00001|"+"00000000||||"+vporc2 +"|000000000000000"
        Else 
        string="|"+xNro_Item+"|"+nprod+"|"+cantunid+"|"+precio+"|"+vporc1+;
            "|M|00001|"+"00000000||||0000|000000000000000"
     endi 

     =Enviar(comando,string)
     sele egretemp
     
     * string="|00001|"+nprod+"|"+cantunid+"|"+precio+"|"+vporc1+;     /// Anterior
     *       "|M|00001|"+"00000000||||"+vporc2
     *- mdife = mbruto - mimpoItem    &&&& Para FInanciamiento o Descuento

     skip 1 
endd

if mdife > 0
   do rfinanci
endif
retu 
**
**************************************************************************

procedure rFACTPAGO
** Imprime Forma de Pago

if mdife < 0
	do rdescuento
endif

importe = mbruto        &&&&  ImpoItem       &&&& VER MARCELO

    mdeci = righ(str(importe-int(importe),4,2),2)
    tpago = righ('0000000'+ allt(str(int(importe),7)),7) + ;
        righ('00'+ allt(mdeci),2)

   comando="@FACTPAGO"

   string="|00001|PAGO|"+tpago+"|T"
   =Enviar(comando,string)

retu 
*
** --------------------------------------------------
procedure rFACTCIERRA
comando="@FACTCIERRA"
string="|00001|F|"+mtipcom+"|FINAL"
=Enviar(comando,string)
retu 
**
** --------------------------------------------------
function enviar
PARAMETERS comando,String

n=fwrite(fp,comando)
n=fputs(fp,string)
return 0
**
*** -------------------
function cierre

*sele estacion
* use c:\estacion\estacion 
*wait ' Oprima una tecla Cuando termine de imprimir !!! ... ' wind 
*set defa to allt(estacion.bases) 

*-sele estacion 
*-use 

* dele file c:\estacion\pfisout.txt 

*if not file("pfisout.txt")
*	wait wind " OJO !!!! - No se encuentra archivo pfisout.txt"
*	return
*endif

sele fiscal
zap

wait wind "Cierre Ventana con el mouse + [ENTER]"
appe from pfisout.txt sdf 

*wait wind "estacion pfisout"
*appe from c:\estacion\pfisout.txt sdf 

** -----------  Controlo si hubo error en Fiscal y grabo Fecha -------------- **
SELE FISCAL
GO TOP

DO WHILE !EOF()
    IF AT('ERROR',detalle) > 0
    	errorfis = .t.
    endif
    skip 1
enddo	

** -----------  Grabo fiscal en d_fiscal y t_fiscal -------------- **
sele Fiscal
GO TOP

sele t_fiscal
set order to tag fis_In
go bott
mni_fis = t_fiscal.ni_fis+1 

appe blank
repl ni_fis  with mNi_fis ,;
     ni_fac  with mNi_fac ,;
     fecha   with date()  ,;
	 hora    with time()  ,;
     proceso with 'FACTURA-' +mtipcom+'-' 


sele Fiscal
GO TOP

DO WHILE !EOF()
	sele d_fiscal
	appe blank
	
	repl ni_fis with mNi_fis ,;
	     detalle with fiscal.detalle 
    
    sele fiscal    
    skip 1
enddo	

sele fiscal
go bott
if empty(detalle)
	errorfis = .t.
endif

*appe blank
*repl detalle with repl('-',80)

** ---------------------------------------

sele fiscal              
go bott

if AT('@FACTCIERRA',detalle) = 0
	wait wind  ' Error en FACTCIERRA   '
	errorfis = .t.
endif

mnrofis = substr(detalle, AT('@FACTCIERRA',detalle)+36,8)

sele t_fiscal
repl nrofis with mnrofis

return
**
** --------------------------------------------------
Procedure RutGrabar
cmensaje='Confirma los datos ingresados ??'

if confirma(cmensaje)=1

  ** grabacion de la factura

  do grabar

  ** ingresa el cobro contado
  do ingcobro.spr
  
  **imprime en la impresora fiscal
  do haceformu
  
endif

do ponencero

*store space(8) to m.nrofact
*store space(1) to m.tipfacvta
*store {} to m.fecfact
*store space(4) to m.codclien,m.nrosuc

store 0 to m.netovta,m.ivavta,m.sobretasa,m.finalvta,m.cantmov,m.precio
store 0 to m.codprod
store space(30) to m.concepto
store space(2) to m.codrub
show gets

_curobj=objnum(m.nrosuc)

return
**
** -------------------------------------
procedure rdescuento
   elprecio = abs(mdife)      &&&&&  +((preunita * mtasaiva )/100)

     if mtipcom ="A"
        elprecio = elprecio / 1.21
     else
        elprecio = elprecio      &&&&&  +((preunita * mtasaiva )/100)
     endif

   mdeci  = righ(str(elprecio-int(elprecio),4,2),2)
   ladife = righ('0000000'+ allt( str(int(elprecio),7)),7) +;
            righ('00'+ allt(mdeci),2)

   comando="@FACTPAGO"
   string="|00001|DESCUENTO|"+ladife+"|D"
   =Enviar(comando,string)

*  @FACTPAGO|00001|DESCUENTO 10%|000027500|D
return

** -------------------------------------
procedure rfinanci
   if mtipcom ="A"
      elprecio = mdife / 1.21
   else
      elprecio = mdife      &&&&&  +((preunita * mtasaiva )/100)
   endif

   mdeci = righ(str(elprecio-int(elprecio),4,2),2)
   ladife = righ('0000000'+ allt( str(int(elprecio),7)),7) +;
            righ('00'+ allt(mdeci),2)


   vporc1=str(mtasaiva,2,0)+"00"
   vporc2="1050"

   comando="@FACTITEM"
   string="|00001|Financiamiento|00001000|"+ladife+"|"+vporc1+;
          "|M|00001|"+"00000000||||"+vporc2
   =Enviar(comando,string)
return
**
** -------------  basura  -----------------

sele forpago 
go top

do case
    case val(forpago.codpag) = 1   &&& allt(forpago.efeche) = "EF"       
       vdespago=alltrim(upper(forpago.nompago))     &&&& "Efectivo"

    case val(forpago.codpag) = 6   &&&  Tarjeta
      vdespago=alltrim(upper(forpago.nompago)) 

    case val(forpago.codpag) = 5   &&& Cheque 3ro.  
         vdespago=alltrim(upper(forpago.nompago)) 

    case allt(forpago.efeche) = "CR"  &&&   Financiado
         vdespago=alltrim(upper(forpago.nompago)) 
endcase
