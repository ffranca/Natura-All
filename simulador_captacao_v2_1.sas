OPTIONS VALIDVARNAME=ANY;

%macro ExtendValidMemName;

%if %sysevalf(&sysver>=9.3) %then options validmemname=extend;

%mend ExtendValidMemName;

%ExtendValidMemName;
/*libname SIMULA "D:\work\Natura Simulador\Dados";*/
/*Ambiente Antigo*/
/*libname SIMULA "D:\SASCONFIG\Lev1\SASApp\Data\natura\PLANEJAMENTO_DSC";*/
libname SIMULA "F:\data\natura\Balanceamento\PLANEJAMENTO_DSC";
/* Classe Main*/
/*%let CLASS_PATH= C:\Users\Fabio\Google Drive\Projetos\Natura\Balanceamento\Programas\v1.01;*/
%let CLASS_PATH=F:\data\natura\Balanceamento\balanceamento_compartilhado\REPOSITORIO STP;
/*options mprint symbolgen mlogic;*/
%include "&CLASS_PATH/CPTLeDados_v2.2.sas";
%include "&CLASS_PATH/CPTLog_v2.sas";

%macro timing;
	current = time();
	put current= time.;
%mend timing;

%macro AjustaCalendario;
/* Faz calendário, captação e relações únicos incluindo períodos de lançamentos (compras com Cartão de Crédito)*/
	/* Ajusta percentual de captação do evento na curva de antecipação*/
	for{<cc,a,st,ds,dt> in CaptaDiaCCSet, d in diaCaptaCC: <(cc),(a),(st),(ds)> in CaptaDiaSet} do;
			percCaptaDiaCC[cc,a,st,ds,dt,d] = percCaptaDiaCC[cc,a,st,ds,dt,d] * 
				(percCaptaCiclo[cc,a,st,ds,dt]) * percCaptaDia[cc,a,st,ds,dt-abreCiclo[cc,a,st]+1];
	end;
	
	/* Ajusta percentual de captação do evento na curva dentro do ciclo*/
	for{<cc,a,st,ds,dt> in CaptaDiaCCSet, d in diaCapta: <(cc),(a),(st),(ds)> in CaptaDiaSet and
			abreCiclo[cc,a,st]+d-1 = dt} do;
			percCaptaDia[cc,a,st,ds,d] = percCaptaDia[cc,a,st,ds,d] * (1-percCaptaCiclo[cc,a,st,ds,dt]);
	end;

	/* Ajusta o calendário*/
	num minCapta;
	for{<cc,a,st,ds,dt> in CaptaDiaCCSet} do;
		minCapta = min{d in diaCaptaCC: percCaptaDiaCC[cc,a,st,ds,dt,d]~=0} d;
		abreCiclo[cc,a,st] = abreCiclo[cc,a,st] + minCapta;
	end;

%mend AjustaCalendario;

/******************** CLASSE Comercial *********************/

/* Public Comercial.init()*/
%macro Comercial_init;
/* Calcula o horizonte de simulação em Dias*/
/* Faz intersecção de calendãrio com demanda com CDs ativos*/
	set Dias init {};
	Dias = (min{<ciclo,ano> in DemandaSet, <cc,a,st> in Calendario: ciclo=cc and a=ano} abreCiclo[cc,a,st])
			..(max{<ciclo,ano> in DemandaSet, <cc,a,st> in Calendario: ciclo=cc and a=ano} fechaCiclo[cc,a,st]);
	Dias = Dias inter union{cd in CDSet} {dataIniCD[cd]..dataFinCD[cd]};
/*	put Dias=;*/
	num firstDay = min{d in Dias} d;
	num lastDay = max{d in Dias} d;
/* Data da extração da estrURa comercial*/
/*- Quantidade de CNs não muda com viradas de chave*/
/*- A virada de chave na estrutURa comercial consiste apenas em mudas setores de GVs*/
/*- Um setor não muda de RE nas viradas de chave*/
	num dataIniEC = min{<st,zn,di> in EstrComSet} di;

/* ZONEAMENTO *==>* SETOR *==>1 GV *==>1 RE*/
	set zoneSet = setof{<st,zn,di> in EstrComSet}<zn>;
	set setorSet = setof{<st,zone,di> in EstrComSet}<st>;
	num reSetor{st in setorSet} init 0;
	str URSetor{st in setorSet} init '';
	for{st in setorSet, <st1,zn,di> in EstrComSet: st=st1 and di=dataIniEC} do;
		reSetor[st] = reEC[st,zn,di];
		URSetor[st] = UREC[st,zn,di];
	end;

/*	print reSetor URSetor;*/

/* Fazer conjunto de GVs para um setor com a data ini e fim da virada de chave*/
	set<num,num,num> gvSetor{st in setorSet} = setof{<st1,zn,di> in EstrComSet: st=st1}
		<gvEC[st,zn,di],di,dataFinEC[st,zn,di]>;

/********************** Faz zoneamento por setor e setors por zoneamento ************************/
	set<str> zoneSetorSet{st in setorSet} = setof{<st1,zn,di> in EstrComSet: st=st1}<zn>;
	set setorZoneSet{zn in zoneSet} = setof{<st,zn1,di> in EstrComSet: zn=zn1}<st>;

	/* Faz ciclos de estratégia*/
	str estrCiclo{DemandaSet};
	for{<cc,ano,st> in Calendario: <cc,ano> in DemandaSet}
		estrCiclo[cc,ano] = estrategiaCal[cc,ano,st];

/********************** Faz ciclo ano por setor ************************/
	set stSet = setof{<st,zn,di> in EstrComSet}<st>;

	set<num,num> cicloAnoSetor{stSet,Dias} init {};
	for{<ciclo,ano> in DemandaSet, st in stSet} do;
		for{d in Dias: d in abreCiclo[ciclo,ano,st]..(fechaCiclo[ciclo,ano,st])} do;
			cicloAnoSetor[st,d] = cicloAnoSetor[st,d] union {<ciclo,ano>};
		end;
	end;

/********************** Faz percentual do setor/zoneamento na região ************************/
	set RegiaoSet = setof{<st,zn,di> in EstrComSet} <reEC[st,zn,di]>;
	num cnRegiao{re in RegiaoSet} = sum{<st,zn,di> in EstrComSet: reEC[st,zn,di]=re and di=dataIniEC} qtdCN[st,zn,di];
	num percCNSetReg{st in setorSet, zn in ZoneSetorSet[st]} = 
		sum{<st1,zn1,di> in EstrComSet: st=st1 and zn=zn1 and di=dataIniEC} qtdCN[st,zn,di]/cnRegiao[reSetor[st]];

/*	print percCNSetReg;*/
%mend Comercial_init;

/******************** CLASSE Logistica *********************/

%macro Logistica_init;
/* Filtra CDs não ativos */
	CDSet = setof{d in Dias, cd in CDSet: d >= dataIniCD[cd] and d<= dataFinCD[cd]}cd;
/* Faz setores por CD por dia*/
	set setorCDSet{cd in CDSet, d in Dias} = setof{<cd1,st,di> in CDSetor: cd = cd1 and 
		d >= di and d <= dataFinCS[cd,st,di]}st;


/******************* Cria conjunto de setor/zoneamento para um CD/Dia **************/
	set<num,num,num,num,num,str> sepSet init {};
	for{d in Dias, <cd,st,di> in CDSetor: cd in CDSet and d>=di and d<=dataFinCS[cd,st,di]}
		for{zn in zoneSetorSet[st], <gv,sdi,sdf> in gvSetor[st], <cc,a> in cicloAnoSetor[(st),(d)]: d>=sdi and d<=sdf}
			sepSet = sepSet union {<cd,cc,a,d,st,zn>};

/* Variáveis privadas*/
	num lresto, lsep, ldia, lhora, lDiaColeta, lHoraColeta, lns, lnsu;
	num ldiafin, lhorafin, lrestoini;
	num lciclo, lano, lgv, ldiacc, ldowcc, lre, lped, lped2, lvol, litem, lrota, lexit, ldiasciclo;
	str lestr;

/*************************** Estruturas por Rota ****************************/

	set Rotas = setof{<rt,zn,di> in RotaZone} rt;
	set<num,num,num> CDDiaRota = CDSet cross Dias cross Rotas;
	num volDiarioRota{CDDiaRota} init 0;
	num itemDiarioRota{CDDiaRota} init 0;
	num volCALRota{CDDiaRota} init 0;

/*************************** Estruturas de captação ****************************/
	/* Loop CD\dia*/
	num pedCapta_calend{sepSet} init 0;
	num volCapta_calend{sepSet} init 0;
	num itemCapta_calend{sepSet} init 0;

	num pedCapta_diario{sepSet} init 0;
	num volCapta_diario{sepSet} init 0;
	num itemCapta_diario{sepSet} init 0;

	num rotaZnDia{sepSet} init 0;
	num gvZnDia{sepSet} init 0;

%mend Logistica_init;

%macro Logistica_demanda0;
/* Calcula a demanda0*/
/* Entrada cd,dia,st,zn*/
/*	put cd= dia= st= zn=;*/
	lre = reSetor[st];
	lped = 0; lvol=0; litem = 0;
	if ciclo ~= 0 then do;
		ldiasciclo = fechaCiclo[ciclo,ano,st]-abreCicloOrg[ciclo,ano,st];
		lestr = estrCiclo[ciclo,ano];
		ldiacc = dia - abreCicloOrg[ciclo,ano,st] + 1;
		if ldiacc > 0 then do;
			ldowcc = weekday(abreCicloOrg[ciclo,ano,st]);
			/* Calcula número de pedidos no dia*/
			lped = demPedido[ciclo,ano,lre];
			lped = lped * percCNSetReg[st,zn];
			lped = lped * percVarDiasCiclo[ldiasciclo];
			lped = lped * percCaptaDia[ciclo,ano,st,ldowcc,ldiacc];
			lvol = lped * percRelDia[ciclo,ano,st,ldowcc,ldiacc];
			litem = lped * percRelItemDia[ciclo,ano,st,ldowcc,ldiacc];
		end;
	end;
	/* Verifica se evento na GV no dia*/
	for{<(ciclo),(ano),(st),ds,dt> in CaptaDiaCCSet, dcc in diaCaptaCC: 
			percCaptaDiaCC[ciclo,ano,st,ds,dt,dcc]>0 and dia-dt=dcc} do;
		put percCaptaDiaCC[ciclo,ano,st,ds,dt,dcc]=;
		lped2 = demPedido[ciclo,ano,lre]*percCNSetReg[st,zn]*percVarDiasCiclo[ldiasciclo]*percCaptaDiaCC[ciclo,ano,st,ds,dt,dcc];
		pedCC[ciclo,ano,st,ds,dt,dcc] = pedCC[ciclo,ano,st,ds,dt,dcc] + lped2;	
		lped = lped + lped2;
		lvol = lvol + lped2 * percRelDiaCC[ciclo,ano,st,ds,dcc];
		litem = litem + lped2 * percRelItemDiaCC[ciclo,ano,st,ds,dcc];
	end;
%mend Logistica_demanda0;

/******************** CLASSE Calendarizacao *********************/
%macro Calendarizacao_init;
/****************************** CALENDARIZAÇÃO *********************************/
/* Cria conjunto de zoneamentos calendarizados para um CD/Dia*/
	set<str,num,num,num> znRotaCAL{CDSet} init {};
	num CALini1, CALfin1, CALini2, CALfin2;
	for{<rt,di> in CalendSet} do;
/*		put rt=;*/
		for{<rt1,zn,di2> in RotaZone: rt=rt1 and di2<=dataFinCAL[rt,di] and dataFinRZ[rt1,zn,di2]>=di} do;
/*			put zn=;*/
			/* faz a interseção*/
			CALini1 = max(di,di2);
			CALfin1 = min(dataFinCAL[rt,di],dataFinRZ[rt1,zn,di2]);
			for{<cd,st,di3> in CDSetor: cd in CDSet and zn in zoneSetorSet[st] and di3<=CALfin1 and dataFinCS[cd,st,di3]>=CALini1} do;
				CALini2 = max(di3,CALini1);
				CALfin2 = min(dataFinCS[cd,st,di3],CALfin1);
				if CALini2 < CALfin2 then 
					znRotaCAL[cd] = znRotaCAL[cd] union {<zn,rt,CALini2,CALfin2>};
			end;
		end;
	end;
/* Conjunto temporário para o loop diário*/
	set<num,str,num> znRotaCALDia init {};
	set rotaCALDia init {};
/* Variáveis de classe*/
	num cDiaSep, cHoraSep, cDiaColeta, cDiaCorte, cHoraCorte, cColeta, crota, cexit, ci;
%mend Calendarizacao_init;

%macro Calendarizacao_separa;
/*	Calcula as demandas CAL para o CD/Dia*/
	/* Faz conjunto de st,zn,rt calendarizados no CD/Dia atual*/
	znRotaCALDia = setof{<zn,rt,di,df> in znRotaCAL[cd], st in setorZoneSet[zn]: 
		dia>=di and dia<=df and card(slice(<cd,*,*,dia,st,zn>,sepSet))>0} <st,zn,rt>;

/* Inicializa todas as estrutURas para o CD/Dia corrente*/
	for{<st,zn,rt> in znRotaCALDia, <ciclo,ano,(st),(zn)> in slice(<cd,*,*,dia,*,*>,sepSet)} do;
		for{<gv,di1,df1> in gvSetor[st]: dia>=di1 and dia<=df1} do;
			%Logistica_demanda0;
			pedCapta_calend[cd,ciclo,ano,dia,st,zn] = lped;
			volCapta_calend[cd,ciclo,ano,dia,st,zn] = lvol;
			itemCapta_calend[cd,ciclo,ano,dia,st,zn] = litem;
			volCALRota[cd,dia,rt] = volCALRota[cd,dia,rt] + lvol;
			rotaZnDia[cd,ciclo,ano,dia,st,zn] = rt;
			gvZnDia[cd,ciclo,ano,dia,st,zn] = gv;
		end;
	end;
%mend Calendarizacao_separa;

/******************** CLASSE SepDiaria *********************/
%macro SepDiaria_init;
/* Variáveis de classe*/
	/* Conjunto de rotas por CD/dia (temporário)*/
	set rotaDiaria init {};
%mend SepDiaria_init;

%macro SepDiaria_separa;
	/* Faz separação dos volumes por hora de captação*/
	rotaCALDia = setof{<zn,rt,di,df> in znRotaCAL[cd], st in setorZoneSet[zn]: 
		dia>=di and dia<=df and card(slice(<cd,*,*,dia,st,zn>,sepSet))>0} <rt>;
	rotaDiaria = Rotas diff rotaCALDia;
	/* Armazena o volume diário por rota*/
	for{rt in rotaDiaria, <ciclo,ano,st,zn> in slice(<cd,*,*,dia,*,*>,sepSet), di2 in slice(<rt,zn,*>,RotaZone): dia>=di2 and dia<=dataFinRZ[rt,zn,di2]} do;
		for{<gv,di1,df1> in gvSetor[st]: dia>=di1 and dia<=df1} do;
			%Logistica_demanda0;
			pedCapta_diario[cd,ciclo,ano,dia,st,zn] = lped;
			volCapta_diario[cd,ciclo,ano,dia,st,zn] = lvol;
			itemCapta_diario[cd,ciclo,ano,dia,st,zn] = litem;
			volDiarioRota[cd,dia,rt] = volDiarioRota[cd,dia,rt] + lvol;
			rotaZnDia[cd,ciclo,ano,dia,st,zn] = rt;
			gvZnDia[cd,ciclo,ano,dia,st,zn] = gv;
		end;
	end;
%mend SepDiaria_separa;
%macro saida_detalhada;
/* Dados detalhados para relatórios*/
	/* Captação */
	create data simula.saida_captacao3 from [COD_CD CICLO ANO DATA COD_SETOR ZONEAMENTO]=
		{<cdi,ciclo,ano,dia,st,zn> in sepSet: volCapta_calend[cdi,ciclo,ano,dia,st,zn]+volCapta_diario[cdi,ciclo,ano,dia,st,zn] > 0 and
			gvZnDia[cdi,ciclo,ano,dia,st,zn] ~= 0} 
		ROTA=rotaZnDia[cdi,ciclo,ano,dia,st,zn] GV=gvZnDia ITEM_CALEND=itemCapta_calend VOLUME_CALEND=volCapta_calend PEDIDO_CALEND=pedCapta_calend
		ITEM_DIARIO=itemCapta_diario VOLUME_DIARIO=volCapta_diario PEDIDO_DIARIO=pedCapta_diario 
		;
	create data simula.saida_evento3 from [CICLO ANO COD_SETOR DIA_SEMANA DATA_EVENTO DIA]=
		{<cc,a,st,ds,dt> in CaptaDiaCCSet, d in diaCaptaCC: pedCC[cc,a,st,ds,dt,d]>0} PEDIDOS_EVENTO=pedCC;
%mend saida_detalhada;

%macro formata_saida_detalhada;
data simula.saida_captacao3;
	set simula.saida_captacao3;
	format DATA date. 
	ITEM_CALEND comma15.2
	VOLUME_CALEND comma12.2
	PEDIDO_CALEND comma12.2
	ITEM_DIARIO comma15.2
	VOLUME_DIARIO comma12.2
	PEDIDO_DIARIO comma12.2
	;
run;
data simula.saida_evento3;
	set simula.saida_evento3;
	format DATA_EVENTO DDMMYYS10. 
	;
run;
%mend formata_saida_detalhada;

/******************** Main *********************/
%macro simula;
proc optmodel;
	num current init 0;
	%timing
	put 'INÍCIO';
	%leDadosCaptacao
	/* Inicialização das classes*/
	%timing
	put 'LEDADOS FIM';
	%AjustaCalendario

	%Comercial_init
	%timing
	put 'INICIALIZAÇÃO COMERCIAL FIM';
	%Logistica_init
	%timing
	put 'INICIALIZAÇÃO LOGÍSTICA FIM';
	%Calendarizacao_init
	%timing
	put 'INICIALIZAÇÃO CALENDARIZAÇÃO FIM';
	%SepDiaria_init
	put 'INICIALIZAÇÃO FIM';
	%timing

	set tDias = firstDay..(firstDay+15);
	for{cd in CDSet} do;
		for{dia in Dias} do;
			%Calendarizacao_separa
		end;
		put 'Calendarização ' cd=;
		%timing
	end;
	put 'CALENDARIZAÇÃO FIM';
	%timing

	for{cd in CDSet} do;
		for{dia in Dias} do;
			%SepDiaria_separa
		end;
		put 'Separação Diária ' cd=;
		%timing
	end;
	put 'DIÁRIO FIM';
	%timing
	%saida_detalhada
	put 'SAÍDA DETALHADA FIM';
	%timing
quit;

%formata_saida_detalhada
%mend simula;
%macro rdd_concilia_item;
/* Item por ano,ciclo e RE*/
PROC SQL;
   CREATE TABLE WORK.RECONC_ITEM_00 AS 
   SELECT t1.ANO, 
          t1.CICLO, 
          t1.RE, 
          /* SUM_of_ITEM_CAPTADO */
            (SUM(t1.ITEM_CAPTADO)) AS SUM_of_ITEM_CAPTADO
      FROM WORK.SAIDA_DEM1_001 t1
      GROUP BY t1.ANO,
               t1.CICLO,
               t1.RE;
QUIT;
/* Demanda item no mesmo formato*/
PROC TRANSPOSE DATA=SIMULA.DEMANDA_ITENS
	OUT=WORK.DEMANDA_ITENS
	PREFIX=ITEM
	NAME=RE
	LABEL=RE
;
	BY ANO CICLO;
	VAR SUL "SP CAPITAL"n "RJ-ES"n MG "SP INTERIOR"n "CENTRO OESTE"n NORDESTE NORTE;

RUN; QUIT;
/* Calcula fator de reconciliação*/
PROC SQL;
   CREATE TABLE WORK.RECONC_ITEM AS 
   SELECT t1.ANO, 
          t1.CICLO, 
          t1.RE, 
          /* FATOR_CONC */
            (t2.ITEM1 / t1.SUM_of_ITEM_CAPTADO) AS FATOR_CONC
      FROM WORK.RECONC_ITEM_00 t1, WORK.DEMANDA_ITENS t2
      WHERE (t1.ANO = t2.ANO AND t1.CICLO = t2.CICLO AND t1.RE = t2.RE);
QUIT;
/* Aplica fator no resultado*/
PROC SQL;
   CREATE TABLE WORK.SAIDA_DEM1_002 AS 
   SELECT t1.COD_CD, 
          t1.CD, 
          t1.DATA, 
          t1.MES, 
          t1.ANO, 
          t1.CICLO, 
          t1.UR, 
          t1.COD_RE, 
          t1.RE, 
          t1.COD_GV, 
          t1.GV, 
          t1.COD_SETOR, 
          t1.SETOR, 
          t1.UF, 
          t1.NOME_UF, 
          t1.CIDADE, 
          t1.ZONEAMENTO, 
          t1.ROTA, 
          t1.TIPO_DEM, 
          /* ITEM_CAPTADO */
            (t1.ITEM_CAPTADO * t2.FATOR_CONC) AS ITEM_CAPTADO, 
          t1.VOLUME_CAPTADO, 
          t1.PEDIDO_CAPTADO
      FROM WORK.SAIDA_DEM1_001 t1, WORK.RECONC_ITEM t2
      WHERE (t1.ANO = t2.ANO AND t1.CICLO = t2.CICLO AND t1.RE = t2.RE);
QUIT;
%mend rdd_concilia_item;

%macro rel_demanda_detalhada;
/* Transforma o input de volume_tipo em volume/tipo*/
data SAIDA_CAPTA_000(drop = item_calend item_diario volume_calend volume_diario pedido_calend pedido_diario);
	set SIMULA.saida_captacao3;
	format TIPO_DEM $6.;
	if volume_calend > 0 then do;
		ITEM = item_calend;
		VOLUME = volume_calend;
		PEDIDO = pedido_calend;
		TIPO_DEM = 'CAL';
		output;
	end;

	if volume_diario > 0 then do;
		ITEM = item_diario;
		VOLUME = volume_diario;
		PEDIDO = pedido_diario;
		TIPO_DEM = 'DIARIO';
		output;
	end;
run;
/* Preciso de uma tabela com o percentual de cada GV/Setor/Zone/dia na rota/dia*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_CAPTA_001 AS SELECT
		t1.COD_CD, 
		t1.DATA,
		t1.ROTA,
		t1.TIPO_DEM, 
		/* SUM_of_VOLUME */
		(SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS VOLUME_CAPTADO
	FROM WORK.SAIDA_CAPTA_000 AS t1
	GROUP BY t1.COD_CD, t1.DATA, t1.ROTA, t1.TIPO_DEM;
QUIT;
/* Junta com o volume captado da rota/tipo para calcular a participação do setor na rota/tipo*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_CAPTA_002 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		t1.DATA,
		t1.ITEM AS ITEM_CAPTADO,
		t1.VOLUME AS VOLUME_CAPTADO,
		t1.PEDIDO AS PEDIDO_CAPTADO, 
		/* PART_SETOR_ROTA */
		(t1.VOLUME/t2.VOLUME_CAPTADO) AS PART_CAPTA_ROTA
	FROM WORK.SAIDA_CAPTA_000 AS t1, SAIDA_CAPTA_001 AS t2
	WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA AND t1.TIPO_DEM=t2.TIPO_DEM)
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.DATA, t1.ROTA, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO;
QUIT;

/* Insere os campos da estrutura comercial*/
PROC SQL;
	CREATE TABLE saida_dem1_001 AS SELECT DISTINCT 
		t1.COD_CD, 
		t3.CD,
		t1.DATA,
		month(t1.DATA) as MES, 
		t1.ANO,
		t1.CICLO, 
		t2.UR, 
		t2.COD_RE, 
		t2.RE, 
		t1.GV AS COD_GV, 
		t2.GV, 
		t1.COD_SETOR, 
		t2.SETOR,
		t2.UF,
		t2.NOME_UF,
		t2.CIDADE,
		t1.ZONEAMENTO,
		t1.ROTA, 
		t1.TIPO_DEM, 
		t1.ITEM_CAPTADO,
		t1.VOLUME_CAPTADO,
		t1.PEDIDO_CAPTADO
	FROM SAIDA_CAPTA_002 AS t1
	inner join SIMULA.ESTRUTURA_COMERCIAL AS t2 
		on t1.GV=t2.COD_GV AND t1.COD_SETOR = t2.COD_SETOR AND t1.ZONEAMENTO=t2.ZONEAMENTO
	inner join  SIMULA.CD AS t3 on t1.COD_CD = t3.COD_CD
	;
QUIT;
/*Reconcilia item captado de acordo com a demanda entrada=saida_dem1_001 saida=saida_dem1_002*/
%rdd_concilia_item
/* Junta com informações de PA e Transportadora*/
PROC SQL;
	CREATE TABLE SIMULA.SAIDA_CAPTACAO_DETALHADA3 AS SELECT DISTINCT 
		t1.*, 
		t2.COD_PA,
		t2.PA,
		t3.TRANSPORTADORA,
		t3.FILIAL
	FROM saida_dem1_002 AS t1
	LEFT join SIMULA.PA_ROTA AS t2 
		on t1.ROTA=t2.ROTA AND t1.DATA >= t2.DATA_INI AND t1.DATA <= t2.DATA_FIN
	LEFT join SIMULA.ROTA_TRANSPORTADORA_FILIAL AS t3 
		on t1.ROTA=t3.ROTA AND t1.DATA >= t3.DATA_INI AND t1.DATA <= t3.DATA_FIN
	;
QUIT;

%mend rel_demanda_detalhada;


%macro main;
%if &erro = 0 %then %do;
	%simula
	%rel_demanda_detalhada
%end;
/* LIMPA O LIXO*/
proc datasets kill nolist;
quit;
%mend main;
%main;