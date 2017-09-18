libname SIMULA "F:\data\natura\Balanceamento\PLANEJAMENTO_DSC";
/*%let _data_ini = 01DEC2013;*/
/*%let _data_fin = 01MAR2014;*/
/*%let _data_ini = 15FEB2015;*/
/*%let _data_fin = 15JUN2015;*/
options symbolgen mprint mlogic;
%macro gera_prazos;
proc optmodel;
/* CADASTRO_FILIAIS*/
	set<str> FilialSet;
	str transpFl{FilialSet};
	read data simula.cadastro_filiais into FilialSet=[FILIAL]
		transpFl=TRANSPORTADORA;
/*	print reFl cidadeFl tempoMovFl tpTurnoFl voloTPZN tempoEntregaFl;*/

/* ITINERARIO*/
	set<num,str,num,num> ItinerarioSet;
	str filialIti{ItinerarioSet};
	num rotaIti{ItinerarioSet};
	str tipoDemIti{ItinerarioSet};
	num dataIti{ItinerarioSet};
	num volIti{ItinerarioSet};
	num tempo_transp_zn{ItinerarioSet};
	num tempo_transp_fl{ItinerarioSet};
	read data work.data_iti_filial into ItinerarioSet=[COD_CD ZONEAMENTO DATA DATA_HORA_EXP]
	filialIti=FILIAL tempo_transp_zn=DURACAO_TOTAL tempo_transp_fl=DURACAO_CD_FILIAL 
	rotaIti=ROTA tipoDemIti=TIPO_DEM dataIti=DATA volIti=VOLUME 
	;

/****************** O programa *************************/
/* data e hora que chega na filial e zn*/
	num filial_in{ItinerarioSet} init 0;
	num filial_out{ItinerarioSet} init 0;
	num zn_in{ItinerarioSet} init 0;

	num tempo;
	for{<cd,zn,dt,dte> in ItinerarioSet: filialIti[cd,zn,dt,dte] in FilialSet} do;
		/* Filial*/
		tempo = tempo_transp_fl[cd,zn,dt,dte];
		filial_in[cd,zn,dt,dte] = intnx('MINUTE',dte, tempo*24*60);
		filial_out[cd,zn,dt,dte] = filial_in[cd,zn,dt,dte];
		/* Cálculo do tempo cd-zn*/
		tempo = tempo_transp_zn[cd,zn,dt,dte];
		zn_in[cd,zn,dt,dte] = intnx('MINUTE',dte, tempo*24*60);
	end;

	create data input_otimizacao_filial from [COD_CD ZONEAMENTO DATA/format=date. DATA_HORA_EXPEDICAO/format=datetime.]=
		{<cd,zn,dt,dte> in ItinerarioSet}
		ROTA=rotaIti TIPO_DEM=tipoDemIti TRANSPORTADORA=(transpFl[filialIti[cd,zn,dt,dte]]) FILIAL=filialIti 
		DATA_ENTREGA=(datepart(zn_in[cd,zn,dt,dte]))/format=date7. 
		DATA_EXP_FL=(datepart(filial_out[cd,zn,dt,dte]))/format=date7. VOLUME=volIti
		filial_in/format=datetime. filial_out/format=datetime. zn_in/format=datetime.;
	;

quit;
%mend gera_prazos;
%macro prepara_input(FL,NDX,first);
proc sql;
	create table data_iti_filial as select * from data_itinerario
	where filial = "&FL." and datepart(data_hora_exp) between "&_data_ini."d and "&_data_fin."d;
quit;
%gera_prazos;
/* Recuperao CD da filial para usar como filtro para feriados na otimização*/
%global _CD;
proc sql noprint;
	select distinct cod_cd into :_CD from input_otimizacao_filial
	where filial = "&FL";
quit;
data input_otm_&NDX.(rename=(data=data_captacao));
	format ROTA	8. ZONEAMENTO $9. TIPO_DEM $6. TRANSPORTADORA $30. FILIAL $50. 
		VOLUME best12.  DATA date. DATA_EXPEDICAO date. DATA_HORA_EXPEDICAO datetime.
		cod_cd 8. data_hora_expedicao datetime. 
		filial_in datetime. filial_out datetime. 
		DATA_ENTREGA date. DATA_EXP_FL date. zn_in datetime.;

	set input_otimizacao_filial;
	DATA_EXPEDICAO = datepart(DATA_HORA_EXPEDICAO);
run;
proc sql;
	create table input_&NDX. as select distinct
		FILIAL,
		ROTA,
		ZONEAMENTO, 

		TIPO_DEM,
		DATA_EXPEDICAO as DATA_SAIDA_CD,
		DATA_ENTREGA AS ZN_IN,
		DATA_EXP_FL AS DATA_SAIDA_ENTREGA,
		0 as TEMPO_ENTREGA,
		TRANSPORTADORA,
		0 AS PRAZO_ENTREGA_UTIL,
		sum(VOLUME) as VOLUME,
		0 as comprometido,
		DATA_ENTREGA
	from input_otm_&NDX.  group by
		ROTA,
		ZONEAMENTO, 
		TIPO_DEM,
		TRANSPORTADORA,
		FILIAL,
		DATA_EXPEDICAO,
		DATA_ENTREGA,
		DATA_EXP_FL;
quit;
%if &first. = 0 %then %do;
data entrega_compr(where= (DATA_ENTREGA >= "&_data_ini."d));
	set backlog&NDX.(rename=(volume_entrega=VOLUME));
	comprometido = 1;
run;
data input_&NDX.;
	set input_&NDX. entrega_compr;
run;
%end;

%mend prepara_input;

%macro otimiza(FL, NDX, first);
%prepara_input(&FL., &NDX., &first.);
proc optmodel printlevel=0;
	set<str,num,str,str,num,num,num,num> EntregaTotSet;
	num volumeTot{EntregaTotSet};
	num comprometidoTot{EntregaTotSet};
	read data input_&NDX. into EntregaTotSet=[FILIAL ROTA ZONEAMENTO TIPO_DEM DATA_SAIDA_CD ZN_IN DATA_SAIDA_ENTREGA TEMPO_ENTREGA]
		volumeTot=volume comprometidoTot=comprometido;
/* O EntregaTotSet contém o comprometido com todas as datas de entrega (bug corrigido)*/
	set<str,num,str,str,num,num,num> EntregaSet = setof{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te> in EntregaTotSet} 
		<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out>;
	num comprometido{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet} = 
		max{te in slice(<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,*>, EntregaTotSet)} comprometidoTot[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
	num volume{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet} = 
		sum{te in slice(<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,*>, EntregaTotSet)} volumeTot[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
	num prazo_contrato{EntregaSet};
	
/*	print prazo;*/
	set<str,str> TrFilial;
	num cap_normal{TrFilial};
	num cap_estr{TrFilial};
	num entrega_sabado{TrFilial};
	num entrega_domingo{TrFilial};
	read data SIMULA.CAPACIDADE_FILIAL(where=(filial="&FL")) into TrFilial=[TRANSPORTADORA FILIAL]
		cap_normal=capacidade_normal cap_estr=capacidade_estrategia 
		entrega_sabado entrega_domingo;

	num cap_entrega_sabado;
	num cap_entrega_domingo;
	num capNormal;
	str transp;
	
	for{<tr,fl> in TrFilial: fl = "&FL."} do;
		cap_entrega_sabado = entrega_sabado[tr,fl];
		cap_entrega_domingo = entrega_domingo[tr,fl];
		capNormal = cap_normal[tr,fl];
		transp = tr;
	end;

/* Feriados*/
	set<num,num> FeriadoSet;
	str descricao{FeriadoSet};
	str tipoFeriado{FeriadoSet};
	read data simula.feriado(where=(COD_CD = &_CD.)) into FeriadoSet=[COD_CD DATA] descricao tipoFeriado=TIPO;
	set Feriado = setof{<cd,data> in FeriadoSet: tipoFeriado[cd,data] = 'F'}<data>;
/*	put Feriado=;*/

/* Carrega Prazo*/
	set<num,str,num,num> RotaZone;
	num prazo_contratado{RotaZone};
	read data SIMULA.ROTA_ZONEAMENTO into RotaZone=[ROTA ZONEAMENTO DATA_INI DATA_FIN]
		prazo_contratado;
	for{<rt1,zn1,di,df> in RotaZone, <fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet: rt1=rt and zn1=zn and cd_out >= di and cd_out <= df}
		prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = prazo_contratado[rt,zn,di,df];

/* Horizonte*/
	set Dias = (min{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet} fl_out)..(max{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet} zn_in+7);
	set Semanas = setof{d in Dias} <year(d)*100+week(d)>;
/*	put Semanas=;*/

/* Quanto vai expedir do volume nos dias 1 até 7 após a chegada no TP*/
	num prazo_max;
	prazo_max = max{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet} (prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out]);
	if prazo_max <= 5 then prazo_max = prazo_max + 2;
	if prazo_max <= 10 then prazo_max = prazo_max + 4;
	else prazo_max = prazo_max + 6;
	set TEntrega = 0..(prazo_max);
	var varQtdExp{EntregaSet,TEntrega} >= 0;

/* Restrição horizonte rolante ==> respeitar o que comprometido*/
	con HR{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te> in EntregaTotSet: comprometidoTot[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = 1}:
		varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = volumeTot[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
		
/* A soma da expedição é igual ao volume inicial*/
	con fluxo{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet}:
		sum{te in TEntrega} varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = volume[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out];
		
/* Restrição de capacidade de entrega*/
	var varCap{Semanas} >= 0;
	con capEnt{d in Dias}:
		sum{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: zn_in+te = d} 
			varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] <= varCap[year(d)*100+week(d)];

/* Restrição de capacidade mínima - usar capacidade normal*/
	con capMin{s in Semanas}:
		varCap[s] >= capNormal;

/* Entrega Sábado*/
	con entrSab{d in Dias: weekday(d) = 7}:
		sum{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: zn_in+te = d} 
			varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] <= varCap[year(d)*100+week(d)]*cap_entrega_sabado;
/* Entrega Domingo*/
	con entrDom{d in Dias: weekday(d) = 1}:
		sum{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: zn_in+te = d} 
			varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] <= varCap[year(d)*100+week(d)]*cap_entrega_domingo;

/* Nova restrição de alisamento capacidade máxima de 2 semanas*/
	num max_sem = max{s in Semanas} s;
	num sem{1..(card(Semanas))};
	num i;
	i = 1;
	for{s in Semanas} do;
		sem[i] = s;
		i = i + 1;
	end;
	set Sem2 = setof{n in 1..(card(Semanas)-1)} <sem[n],sem[n+1]>;
/*	put Sem2=;*/
	var varCap2{Sem2} >= 0;
	con Alisa1{<s1,s2> in Sem2}: varCap2[s1,s2] >= varCap[s1]-varCap[s2];
	con Alisa2{<s1,s2> in Sem2}: varCap2[s1,s2] >= varCap[s2]-varCap[s1];


/* Não pode atrasar*/
	num prazo_entrega{EntregaSet,TEntrega};
	set diasUteis;
	for{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega} do;
		diasUteis = setof{d in cd_out..(zn_in+te): weekday(d) in 2..6} <d>;
		diasUteis = diasUteis diff Feriado;
		prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = card(diasUteis)-1;
	end;
	num altera_prazo{EntregaSet} init 0;
	set teUtil{EntregaSet} init {};
	num dia_entrega, dia_saida, saida_ok, entrega_ok;
	/* Reduz domínio te para demanda diaria e calendarizada (v1.0.4)*/
	for{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: tp_dem='DIARIO' or tp_dem='CAL'} do;
		dia_entrega = zn_in+te;
		/*Dentro do prazo contratado em dias úteis?*/
		if prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] > prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] then do;
			/* Se não tiver alternativa altera o prazo de entrega*/
			if card(teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out]) = 0 then do;
				prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
				altera_prazo[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = 1;
			end;
		end;
		if prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] <= prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] then do;
			entrega_ok = 0;
			if (dia_entrega not in Feriado) then do;
				if weekday(dia_entrega) in 2..6 then entrega_ok=1;
				else if weekday(dia_entrega) = 7 and cap_entrega_sabado > 0 then entrega_ok=1;
				else if weekday(dia_entrega) = 1 and cap_entrega_domingo > 0 then entrega_ok=1;

				if entrega_ok then
					teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] union {te};
			end;
		end;
	end;
	/* Reduz domínio te para entrega expressa*/
	for{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: tp_dem='EE'} do;
		dia_entrega = zn_in+te;
		/*Entrega expressa entrega no menor prazo*/
		if prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] > prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] then do;
			/* Se não tiver alternativa altera o prazo de entrega*/
			if card(teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out]) = 0 then do;
				prazo_contrato[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = prazo_entrega[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
				altera_prazo[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = 1;
			end;
		end;
		if card(teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out]) = 0 then do;
			entrega_ok = 0;
			if (dia_entrega not in Feriado) then do;
				if weekday(dia_entrega) in 2..6 then entrega_ok=1;
				else if weekday(dia_entrega) = 7 and cap_entrega_sabado > 0 then entrega_ok=1;
				else if weekday(dia_entrega) = 1 and cap_entrega_domingo > 0 then entrega_ok=1;

				if entrega_ok then
					teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] union {te};
			end;
		end;
	end;

	con semprazo_entrega{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: te not in teUtil[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out]}:
		varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = 0;

/* Objetivo minimizar capacidade*/
	min obj = sum{s in Semanas} varCap[s]*10 + sum{<s1,s2> in Sem2} varCap2[s1,s2]
				+ sum{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega}
					(varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te]*te/10000)
	; 
	solve with lp/MAXTIME = 300;

/* Monta a saída*/

/*	CREATE TABLE SIMULA.SAIDA_CAP_FILIAL*/
/*	(TRANSPORTADORA CHAR(30), FILIAL CHAR(50), ROTA NUM, TIPO_DEM CHAR(6), DATA DATE, MES NUM, ANO NUM,*/
/*		DEMANDA NUM, DESPACHO NUM, CAPACIDADE_UTIL NUM, CAPACIDADE_NORMAL NUM, CAPACIDADE_ESTRATEGIA NUM);*/
/*	num qtdExp{EntregaSet,TEntrega} init 0;*/
/*	for{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet,te in TEntrega}*/
/*		qtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] = varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];*/
	set<str,num,str,num> capSet = setof{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, d in Dias} <fl,rt,tp_dem,d>;
	num mesCS{capSet};
	num anoCS{capSet};
	num demCS{capSet} init 0;
	num entrCS{capSet} init 0;
	num backlogCS{capSet} init 0;
	num dini = min{d in Dias}d;
	for{<fl,rt,tp_dem,d> in capSet} do;
		mesCS[fl,rt,tp_dem,d] = month(d);
		anoCS[fl,rt,tp_dem,d] = year(d);
		demCS[fl,rt,tp_dem,d] = sum{<zn,cd_out,zn_in> in slice(<fl,rt,*,tp_dem,*,*,d>,EntregaSet), te in TEntrega} 
			varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,d,te];
		entrCS[fl,rt,tp_dem,d] = sum{<zn,cd_out,zn_in,fl_out> in slice(<fl,rt,*,tp_dem,*,*,*>,EntregaSet), te in TEntrega: 
			zn_in+te = d } varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te];
		if d = dini then 
			backlogCS[fl,rt,tp_dem,d] = demCS[fl,rt,tp_dem,d] - entrCS[fl,rt,tp_dem,d];
		else 
			backlogCS[fl,rt,tp_dem,d] = backlogCS[fl,rt,tp_dem,d-1] + demCS[fl,rt,tp_dem,d] - entrCS[fl,rt,tp_dem,d];
		if abs(backlogCS[fl,rt,tp_dem,d]) < 0.01 then backlogCS[fl,rt,tp_dem,d] = 0;
	end;
	create data SAIDA_CAP_&NDX. from [FILIAL ROTA TIPO_DEM DATA]={<fl,rt,tp_dem,d> in capSet}
		TRANSPORTADORA=(transp)
		MES=mesCS ANO=anoCS
		DEMANDA=demCS
		ENTREGA=entrCS
		CAPACIDADE_UTIL = (varCap[year(d)*100+week(d)])
		CAPACIDADE_NORMAL=(cap_normal[transp,"&FL."])
		CAPACIDADE_ESTRATEGIA=(cap_estr[transp,"&FL."])
		BACKLOG=backlogCS
	;
	create data log_altera_prazo_&NDX. from [FILIAL ROTA ZONEAMENTO TIPO_DEM DATA_SAIDA_CD ZN_IN DATA_SAIDA_ENTREGA]=
		{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet: comprometido[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = 0 and altera_prazo[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = 1}
			PRAZO_ALTERADO=prazo_contrato;
		
	/* Saída completa*/
	create data backlog&NDX. from [FILIAL ROTA ZONEAMENTO TIPO_DEM DATA_SAIDA_CD ZN_IN DATA_SAIDA_ENTREGA TEMPO_ENTREGA]=
		{<fl,rt,zn,tp_dem,cd_out,zn_in,fl_out> in EntregaSet, te in TEntrega: varQtdExp[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out,te] > 0 and 
			comprometido[fl,rt,zn,tp_dem,cd_out,zn_in,fl_out] = 0}
			TRANSPORTADORA=(transp) PRAZO_ENTREGA_UTIL=prazo_entrega VOLUME_ENTREGA=varQtdExp DATA_ENTREGA=(zn_in+te)
		;
quit;
DATA backlog&ndx.;
	format TRANSPORTADORA $30. FILIAL $50. ROTA 8. ZONEAMENTO $9. TIPO_DEM $6. DATA_SAIDA_CD date. DATA_SAIDA_ENTREGA date.
	ZN_IN date. DATA_ENTREGA date. VOLUME_ENTREGA 8.2 PRAZO_ENTREGA_UTIL 8.;
	SET backlog&ndx.;
run;

%mend otimiza;


%macro preProc(CD);

/* Transforma saida_captacao em tipo_dem*/
PROC SQL;
   CREATE TABLE WORK.CAPTACAO_001 AS 
   SELECT t1.COD_CD, 
          t1.DATA, 
          t1.ROTA, 
          /* SUM_of_VOLUME_DIARIO */
            (SUM(t1.VOLUME_DIARIO)+SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_RT,
          /* SUM_of_PEDIDO_DIARIO */
            (SUM(t1.PEDIDO_DIARIO)+SUM(t1.PEDIDO_CALEND)) FORMAT=COMMA12.2 AS PEDIDO_RT
      FROM SIMULA.SAIDA_CAPTACAO t1
      GROUP BY t1.COD_CD, t1.DATA, t1.ROTA
      HAVING (CALCULATED VOLUME_RT) > 0 
%if %length(&CD.) > 0 %then %do; 
and cod_cd = &CD.
%end;
	;
QUIT;

PROC SQL;
   CREATE TABLE WORK.CAPTACAO_002 AS 
   SELECT t1.COD_CD, 
          t1.DATA, 
          t1.COD_SETOR, 
          t1.ROTA, 
          t1.ZONEAMENTO, 
          /* SUM_of_VOLUME_DIARIO */
            (SUM(t1.VOLUME_DIARIO)+SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_ZN,
          /* SUM_of_PEDIDO_DIARIO */
            (SUM(t1.PEDIDO_DIARIO)+SUM(t1.PEDIDO_CALEND)) FORMAT=COMMA12.2 AS PEDIDO_ZN
      FROM SIMULA.SAIDA_CAPTACAO  t1
      GROUP BY t1.COD_CD, t1.DATA, t1.COD_SETOR, t1.ROTA, t1.ZONEAMENTO
      HAVING (CALCULATED VOLUME_ZN) > 0 
%if %length(&CD.) > 0 %then %do; 
and cod_cd = &CD.
%end;
	;
QUIT;
PROC SQL;
   CREATE TABLE WORK.PERC_ZN_ROTA AS 
   SELECT t1.COD_CD, 
          t1.DATA, 
          t1.COD_SETOR, 
          t1.ROTA, 
          t1.ZONEAMENTO, 
          /* PERC_ZN_ROTA */
          t1.VOLUME_ZN/t2.VOLUME_RT AS PERC_ZN_ROTA,
		  t2.PEDIDO_RT/t2.VOLUME_RT AS REL_PEDIDO
      FROM WORK.CAPTACAO_002 t1
		INNER JOIN WORK.CAPTACAO_001 t2
      ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA)
	;
QUIT;
PROC SQL;
   CREATE TABLE WORK.SAIDA_NS_PONDERADO AS SELECT DISTINCT
		t1.COD_CD, 
          t1.DATA, 
          t1.ROTA,
		  t1.TIPO_DEM,
          /* NS */
            (SUM(t1.NS*t1.VOLUME)/(t2.VOLUME_RT)) AS NS, 
          /* VOLUME */
            (SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS VOLUME
      FROM SIMULA.SAIDA_NS_DETALHADO t1
	  INNER JOIN WORK.CAPTACAO_001 t2
      ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA)
      GROUP BY t1.COD_CD, t1.DATA, t1.ROTA,t1.TIPO_DEM
%if %length(&CD.) > 0 %then %do; 
	having t1.cod_cd=&CD.
%end;
	;
QUIT;
PROC SQL;
   CREATE TABLE WORK.SAIDA_NS_ZNDIA AS 
   SELECT t1.COD_CD, 
          t1.DATA, 
          t1.COD_SETOR, 
          t1.ROTA, 
          t1.ZONEAMENTO,
          t2.TIPO_DEM,	 
          t2.NS, 
          /* VOLUME */
            (t1.PERC_ZN_ROTA*t2.VOLUME) AS VOLUME,
          /* PEDIDO */
            (t1.REL_PEDIDO*t1.PERC_ZN_ROTA*t2.VOLUME) AS PEDIDO,
		  t3.tempo_separacao,
		  t3.tempo_expedicao,
		  t4.ns_cc
      FROM WORK.PERC_ZN_ROTA t1, WORK.SAIDA_NS_PONDERADO t2, simula.tempo_linha_separacao t3, simula.ns_callcenter t4
      WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA and t1.cod_cd=t3.cod_cd);
QUIT;
/*Calcular hora de expedição data + NS + 6 hs ==> se for domingo joga para 6:00 de segunda*/
data DATA_HORA_EXP_ZN;
	set WORK.SAIDA_NS_ZNDIA;
	format data_hora_exp DATETIME. data date.;
	data_hora_exp = dhms(data,ns_cc+tempo_separacao+NS+tempo_expedicao,0,0);
	if weekday(datepart(data_hora_exp)) = 1 then
		data_hora_exp = dhms(datepart(data_hora_exp)+1,6,0,0);
RUN;
/* Corrige data exp para CAL*/
PROC SQL;
   CREATE TABLE WORK.DATA_HORA_EXP_ZN_0001 AS 
   SELECT DISTINCT t1.*, 
          t2.*
      FROM WORK.DATA_HORA_EXP_ZN t1 left JOIN SIMULA.CALENDARIZACAO t2 ON (t1.ROTA = t2.ROTA)
	and data between data_inicio and data_final;
QUIT;
data DATA_HORA_EXP_ZN(keep= COD_CD DATA COD_SETOR ROTA ZONEAMENTO TIPO_DEM  NS 
	VOLUME PEDIDO TEMPO_SEPARACAO TEMPO_EXPEDICAO ns_cc data_hora_exp);
	set DATA_HORA_EXP_ZN_0001;
	format dexp date. d date. data_hora_exp datetime.;
	array separa{7} dom seg ter qua qui sex sab;

	if tipo_dem = 'CAL' then do;
		dexp = datepart(data_hora_exp);
		do d=dexp to dexp+6;
			if separa{weekday(d)} ~= "" then leave;
		end;
		data_hora_exp = dhms(d+dia_coleta,hour(hora_coleta),0,0);
	end;
run;
/* Alteração no transporte*/
PROC SQL;
   CREATE TABLE WORK.DATA_ITINERARIO AS 
   SELECT distinct t2.ROTA, 
          t2.CD AS COD_CD, 
          t2.ZONEAMENTO, 
          t1.TIPO_DEM, 
          t1.DATA, 
          t1.data_hora_exp, 
            (SUM(t1.VOLUME)) AS VOLUME, 
          t2.FILIAL,
          t2.DURACAO_TOTAL, 
          t2.DURACAO_CD_FILIAL
      FROM DATA_HORA_EXP_ZN t1, SIMULA.ITINERARIO t2
      WHERE (t1.COD_CD = t2.CD AND t1.ROTA=t2.ROTA AND t1.ZONEAMENTO = t2.ZONEAMENTO)
      GROUP BY t1.ROTA, t2.CD, t2.ZONEAMENTO, t1.TIPO_DEM, t1.DATA, t1.data_hora_exp,t2.FILIAL;
QUIT;
%mend preProc;

%macro rel_politica_servico;
/* Modificação v1.0.4 ==> mudar cálculo de dias úteis considerando sábado dia útil*/
data saida_ns_transp_000;
	set simula.saida_ns_transp;

	prazo_entrega_util = prazo_entrega_util + intck('week',DATA_SAIDA_CD,DATA_ENTREGA);
	if weekday(DATA_ENTREGA)=7 then prazo_entrega_util=prazo_entrega_util+1;
run;

/*	Junta WORK.DATA_HORA_EXP com saida_ns_transp pelo dia de expedicao, zoneamento e tipo de demanda*/
/*	ou seja para um dia de expedicao podemos ter um acarga que foi entregue 10% com 0 backlog 30% com*/
/*	backlog = 1 e 60% com backlog = 2*/
/*	para esta carga o backlog medio ponderado foi de 1,5 dias = 36hs*/
/* Vamos comecar gerando a ponderacao de saida_ns_transp*/
/*No final precisamos do ns_transporte (tp+bk+te)-exp(dias corridos) ou tempo_entrega (dias uteis) ponderado*/
/* Ponderacao*/
/*V1.0.4 ==> somar 12 horas ao n. de dias*/
PROC SQL;
 CREATE TABLE WORK.NS_TRANSP_001 AS SELECT t1.ZONEAMENTO,
	 t1.TIPO_DEM,
	 t1.DATA_SAIDA_CD AS DATA_EXPEDICAO,
	 (sum((t1.VOLUME_ENTREGA *(ZN_IN-DATA_SAIDA_CD+0.5))*24)/sum(t1.volume_entrega)) AS NS_TRANSPORTE,
	 (sum((t1.VOLUME_ENTREGA *(DATA_ENTREGA-DATA_SAIDA_CD+0.5))*24)/sum(t1.volume_entrega)) AS NS_TRANSP_CAPFILIAL,
	 (sum((t1.VOLUME_ENTREGA *(t1.PRAZO_ENTREGA_UTIL+0.5))*24)/sum(t1.volume_entrega)) AS NS_TRANSP_CAP_UTEIS
 FROM SIMULA.SAIDA_NS_TRANSP AS t1
 GROUP BY t1.ZONEAMENTO, t1.TIPO_DEM, t1.DATA_SAIDA_CD;
QUIT;
/* Prepara data_hora_exp*/
/* A saida_ns_detalhado temos o NS por rota/tipo de demanda*/								
/* Na tabela de przados de transportes temos rota/zn ==> precisamos fazer a ponderação por zn*/								
/* Esta parte já está feita no rel_ns_pick_transp e podemos reutilizar parte já que precisamos tipo de demanda*/								
/* Depois precisamos colocar em termos de setor como foi feito em relatorio_ns*/								
/* E no final, a partir de saida_captacao calculamos o número de pedidos*/								
								
/* Transforma saida_captacao em tipo_dem*/								
PROC SQL;								
   CREATE TABLE WORK.CAPTACAO_001 AS 								
   SELECT t1.COD_CD, 								
          t1.DATA, 								
          t1.ROTA, 								
          /* SUM_of_VOLUME_DIARIO */								
            (SUM(t1.VOLUME_DIARIO)+SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_RT,								
          /* SUM_of_PEDIDO_DIARIO */								
            (SUM(t1.PEDIDO_DIARIO)+SUM(t1.PEDIDO_CALEND)) FORMAT=COMMA12.2 AS PEDIDO_RT								
      FROM SIMULA.SAIDA_CAPTACAO t1								
      GROUP BY t1.COD_CD, t1.DATA, t1.ROTA								
      HAVING (CALCULATED VOLUME_RT) > 0;								
QUIT;								
								
PROC SQL;								
   CREATE TABLE WORK.CAPTACAO_002 AS 								
   SELECT t1.COD_CD, 								
          t1.DATA, 								
          t1.COD_SETOR, 								
          t1.ROTA, 								
          t1.ZONEAMENTO, 								
          /* SUM_of_VOLUME_DIARIO */								
            (SUM(t1.VOLUME_DIARIO)+SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_ZN,								
          /* SUM_of_PEDIDO_DIARIO */								
            (SUM(t1.PEDIDO_DIARIO)+SUM(t1.PEDIDO_CALEND)) FORMAT=COMMA12.2 AS PEDIDO_ZN								
      FROM SIMULA.SAIDA_CAPTACAO  t1								
      GROUP BY t1.COD_CD, t1.DATA, t1.COD_SETOR, t1.ROTA, t1.ZONEAMENTO								
      HAVING (CALCULATED VOLUME_ZN) > 0;								
QUIT;								
PROC SQL;								
   CREATE TABLE WORK.PERC_ZN_ROTA AS 								
   SELECT t1.COD_CD, 								
          t1.DATA, 								
          t1.COD_SETOR, 								
          t1.ROTA, 								
          t1.ZONEAMENTO, 								
          /* PERC_ZN_ROTA */								
          t1.VOLUME_ZN/t2.VOLUME_RT AS PERC_ZN_ROTA,								
		  t2.PEDIDO_RT/t2.VOLUME_RT AS REL_PEDIDO						
      FROM WORK.CAPTACAO_002 t1								
		INNER JOIN WORK.CAPTACAO_001 t2						
      ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA)								
	;							
QUIT;								
PROC SQL;								
   CREATE TABLE WORK.SAIDA_NS_PONDERADO AS SELECT DISTINCT								
		t1.COD_CD, 						
          t1.DATA, 								
          t1.ROTA,								
		  t1.TIPO_DEM,						
          /* NS */								
            (SUM(t1.NS*t1.VOLUME)/(t2.VOLUME_RT)) AS NS, 								
          /* VOLUME */								
            (SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS VOLUME								
      FROM SIMULA.SAIDA_NS_DETALHADO t1								
	  left JOIN WORK.CAPTACAO_001 t2							
      ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA)								
      GROUP BY t1.COD_CD, t1.DATA, t1.ROTA,t1.TIPO_DEM;								
QUIT;	

PROC SQL;								
   CREATE TABLE WORK.SAIDA_NSU_PONDERADO AS SELECT DISTINCT								
		t1.COD_CD, 						
          t1.DATA, 								
          t1.ROTA,								
		  t1.TIPO_DEM,						
          /* NS */								
            (SUM(t1.NS*t1.VOLUME)/(t2.VOLUME_RT)) AS NS, 								
          /* VOLUME */								
            (SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS VOLUME								
      FROM SIMULA.SAIDA_NSU_DETALHADO t1								
	  left JOIN WORK.CAPTACAO_001 t2							
      ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA)								
      GROUP BY t1.COD_CD, t1.DATA, t1.ROTA,t1.TIPO_DEM;								
QUIT;	

PROC SQL;
   CREATE TABLE WORK.SAIDA_NS_PONDERADO_001 AS 
   SELECT DISTINCT t1.COD_CD, 
          t1.DATA, 
          t1.ROTA, 
          t1.TIPO_DEM, 
          t1.NS, 
          t2.NS AS NSU, 
          t1.VOLUME
      FROM WORK.SAIDA_NS_PONDERADO t1, WORK.SAIDA_NSU_PONDERADO t2
      WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA AND t1.TIPO_DEM = t2.TIPO_DEM);
QUIT;
PROC SQL;								
   CREATE TABLE WORK.SAIDA_NS_ZNDIA AS 								
   SELECT t1.COD_CD, 								
          t1.DATA, 								
          t1.COD_SETOR, 								
          t1.ROTA, 								
          t1.ZONEAMENTO,								
          t2.TIPO_DEM,	 							
          t2.NS, 								
          t2.NSU, 								
          /* VOLUME */								
            (t1.PERC_ZN_ROTA*t2.VOLUME) AS VOLUME,								
          /* PEDIDO */								
            (t1.REL_PEDIDO*t1.PERC_ZN_ROTA*t2.VOLUME) AS PEDIDO,								
		  t3.tempo_separacao,						
		  t3.tempo_expedicao,						
		  t4.ns_cc						
      FROM WORK.PERC_ZN_ROTA t1, WORK.SAIDA_NS_PONDERADO_001 t2, simula.tempo_linha_separacao t3, simula.ns_callcenter t4								
      WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA and t1.cod_cd=t3.cod_cd);								
QUIT;								
/*Calcular hora de expedição data + NS + 6 hs ==> se for domingo joga para 6:00 de segunda*/								
data DATA_HORA_EXP_ZN;								
	set WORK.SAIDA_NS_ZNDIA;							
	format data_hora_exp DATETIME.;							
	data_hora_exp = dhms(data,NS,0,0);							
	if weekday(datepart(data_hora_exp)) = 1 then							
		data_hora_exp = dhms(datepart(data_hora_exp)+1,6,0,0);						
RUN;								
/* Corrige data exp para CAL*/
PROC SQL;
   CREATE TABLE WORK.DATA_HORA_EXP_ZN_0001 AS 
   SELECT t1.*, 
          t2.*
      FROM WORK.DATA_HORA_EXP_ZN t1 left JOIN SIMULA.CALENDARIZACAO t2 ON (t1.ROTA = t2.ROTA);
QUIT;
data DATA_HORA_EXP_ZN(keep= COD_CD DATA COD_SETOR ROTA ZONEAMENTO TIPO_DEM  NS NSU
	VOLUME PEDIDO TEMPO_SEPARACAO TEMPO_EXPEDICAO ns_cc data_hora_exp);
	set DATA_HORA_EXP_ZN_0001;
	format dexp date. d date. data_hora_exp datetime.;
	array separa{7} dom seg ter qua qui sex sab;

	if tipo_dem = 'CAL' then do;
		dexp = datepart(data_hora_exp);
		do d=dexp to dexp+6;
			if separa{weekday(d)} ~= "" then leave;
		end;
		data_hora_exp = dhms(d+dia_coleta,hour(hora_coleta),0,0);
	end;
run;

PROC SQL;
 CREATE TABLE WORK.NS_TRANSP_002 AS SELECT 
	 t1.COD_CD,
	 t1.DATA,
	 t1.COD_SETOR,
	 t1.ROTA,
	 t1.ZONEAMENTO,
	 t1.TIPO_DEM,
	 t1.NS,
	 t1.NSU,
	 t1.VOLUME,
	 t1.PEDIDO,
	 t1.TEMPO_SEPARACAO,
	 t1.TEMPO_EXPEDICAO,
	 t1.ns_cc,
	 t1.data_hora_exp,
	 t2.NS_TRANSPORTE,
	 t2.NS_TRANSP_CAPFILIAL,
	 t2.NS_TRANSP_CAP_UTEIS 
 FROM WORK.DATA_HORA_EXP_ZN AS t1,  WORK.NS_TRANSP_001 AS t2
 WHERE (t1.ZONEAMENTO = t2.ZONEAMENTO AND t1.TIPO_DEM = t2.TIPO_DEM AND datepart(t1.data_hora_exp) = t2.DATA_EXPEDICAO);
QUIT;

PROC SQL;								
   CREATE TABLE NS_TRANSP_003 AS 								
   SELECT DISTINCT t2.COD_CD, 								
          t2.CD, 								
          t2.DATA, 								
          MONTH(t2.DATA) AS MES, 								
          YEAR(t2.DATA) AS ANO_MES, 								
          t2.ANO AS ANO_CICLO, 								
          t2.CICLO, 								
          t2.UR, 								
          t2.COD_RE, 								
          t2.RE, 								
          t2.COD_GV, 								
          t2.GV, 								
          t2.COD_SETOR, 								
          t2.SETOR, 								
          t2.UF, 								
          t2.NOME_UF, 								
          t2.CIDADE, 								
          t2.ZONEAMENTO, 								
          t2.ROTA, 								
          t2.TRANSPORTADORA, 								
          t2.FILIAL, 								
          t1.TIPO_DEM, 								
          t1.VOLUME, 								
          /* PEDIDO */								
			t1.PEDIDO, 					
          /* NS_CAPTACAO */								
            t1.ns_cc AS NS_CAPTACAO, 								
          /* NS_PICKING */								
            (t1.NS) AS NS_PICKING, 								
          /* NSU_PICKING */								
            (t1.NSU) AS NSU_PICKING, 								
          /* NS_TRANSPORTE */								
            t1.NS_TRANSPORTE, 								
          /* NS_TOTAL */								
/*            ((t1.ns_cc) + (t1.NS + t1.tempo_separacao + t1.tempo_expedicao)+ NS_TRANSPORTE) AS NS_TOTAL,*/
            t1.NS_TRANSP_CAPFILIAL, 								
            t1.NS_TRANSP_CAP_UTEIS, 								
/*            ((t1.ns_cc) + (t1.NS + t1.tempo_separacao + t1.tempo_expedicao)+ NS_TRANSP_CAPFILIAL) AS NS_TOTAL_CAPFILIAL,*/
/*            ((t1.ns_cc) + (t1.NS + t1.tempo_separacao + t1.tempo_expedicao)+ NS_TRANSP_CAP_UTEIS) AS NS_TOTAL_CAP_UTEIS,*/
            ROUND((t1.NS + NS_TRANSPORTE)/24) AS PRAZO_DIAS,
            ROUND((t1.NS + NS_TRANSP_CAPFILIAL)/24) AS PRAZO_DIAS_CAPFILIAL,
            ROUND((t1.NS + NS_TRANSP_CAP_UTEIS)/24) AS PRAZO_DIAS_CAP_UTEIS
      FROM WORK.NS_TRANSP_002 t1, SIMULA.SAIDA_DEMANDA_DETALHADA t2								
      WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.COD_SETOR=t2.COD_SETOR AND t1.ROTA = t2.ROTA 								
		AND t1.ZONEAMENTO = t2.ZONEAMENTO AND t2.VOLUME_CAPTADO>0);						
QUIT;								
/* Fazer só por mes/ciclo => tirar data. Precisa ponderar NS*/								
PROC SQL;								
	CREATE TABLE NS_TRANSP_004 AS SELECT 							
		t1.COD_CD,						
		t1.MES,						
		t1.ANO_MES,						
		t1.ANO_CICLO,						
		t1.CICLO,						
		t1.COD_GV,						
		t1.COD_SETOR,						
		t1.ROTA,						
		t1.ZONEAMENTO,						
		t1.TIPO_DEM,						
		 (SUM(t1.VOLUME)) AS SUM_OF_VOLUME,						
		 (SUM(t1.PEDIDO)) AS SUM_OF_PEDIDO 						
	FROM NS_TRANSP_003 AS t1							
	GROUP BY t1.COD_CD, t1.MES, t1.ANO_MES, t1.ANO_CICLO, t1.CICLO, t1.COD_GV, t1.COD_SETOR, t1.ROTA, t1.ZONEAMENTO, t1.TIPO_DEM;							
QUIT;								
PROC SQL;								
   CREATE TABLE NS_TRANSP_005 AS 								
   SELECT t1.COD_CD, 								
          t1.CD, 								
          t1.DATA, 								
          t1.MES, 								
          t1.ANO_MES, 								
          t1.ANO_CICLO, 								
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
          t1.TRANSPORTADORA, 								
          t1.FILIAL, 								
          t1.TIPO_DEM, 								
          t1.VOLUME, 								
          t1.PEDIDO, 								
          /* NS_CAPTACAO */								
            (t1.NS_CAPTACAO*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_CAPTACAO, 								
          /* NS_PICKING */								
            (t1.NS_PICKING*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_PICKING, 								
          /* NSU_PICKING */								
            (t1.NSU_PICKING*t1.VOLUME/t2.SUM_OF_VOLUME) AS NSU_PICKING, 								
          /* NS_TRANSPORTE */								
            (t1.NS_TRANSPORTE*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_TRANSPORTE,								
          /* NS_TRANSP_CAPFILIAL */		
            (t1.NS_TRANSP_CAPFILIAL *t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_TRANSPORTE_CAPFILIAL ,								
          /* NS_TRANSP_CAP_UTEIS */								
            (t1.NS_TRANSP_CAP_UTEIS*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_TRANSP_CAP_UTEIS,								
          /* NS_TOTAL */								
/*            (t1.NS_TOTAL*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_TOTAL,*/
          /* NS_TOTAL_CAPFILIAL */								
/*            (t1.NS_TOTAL_CAPFILIAL*t1.VOLUME/t2.SUM_OF_VOLUME) AS NS_TOTAL_CAPFILIAL,*/
          /* PRAZO_DIAS */								
            (t1.PRAZO_DIAS*t1.VOLUME/t2.SUM_OF_VOLUME) AS PRAZO_DIAS,								
          /* PRAZO_DIAS_CAPFILIAL */								
            (t1.PRAZO_DIAS_CAPFILIAL*t1.VOLUME/t2.SUM_OF_VOLUME) AS PRAZO_DIAS_CAPFILIAL,
          /* PRAZO_DIAS_CAP_UTEIS */								
            (t1.PRAZO_DIAS_CAP_UTEIS *t1.VOLUME/t2.SUM_OF_VOLUME) AS PRAZO_DIAS_CAP_UTEIS								
      FROM NS_TRANSP_003 t1, NS_TRANSP_004 t2								
      WHERE (t1.COD_CD = t2.COD_CD AND t1.MES = t2.MES AND t1.ANO_MES = t2.ANO_MES AND t1.ANO_CICLO = t2.ANO_CICLO AND t1.CICLO = t2.CICLO AND t1.COD_GV = t2.COD_GV AND t1.COD_SETOR								
            = t2.COD_SETOR AND t1.ROTA = t2.ROTA AND t1.ZONEAMENTO = t2.ZONEAMENTO AND t1.TIPO_DEM = t2.TIPO_DEM);								
QUIT;								
PROC SQL;								
   CREATE TABLE SIMULA.SAIDA_POLITICA_SERVICO_CAPFILIAL AS 								
   SELECT DISTINCT t1.COD_CD, 								
          t1.CD, 								
          t1.MES, 								
          t1.ANO_MES, 								
          t1.ANO_CICLO, 								
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
          t1.TRANSPORTADORA, 								

          t1.FILIAL, 								
          t1.TIPO_DEM, 								
          /* VOLUME */								
            (SUM(t1.VOLUME)) AS VOLUME, 								
          /* PEDIDO */								
            (SUM(t1.PEDIDO)) AS PEDIDO, 								
          /* NS_CAPTACAO */								
            (SUM(t1.NS_CAPTACAO)) AS NS_CAPTACAO, 								
          /* NS_PICKING */								
            (SUM(t1.NS_PICKING)) AS NS_PICKING, 								
          /* NSU_PICKING */								
            (SUM(t1.NSU_PICKING)) AS NSU_PICKING, 								
          /* NS_TRANSPORTE */								
            (SUM(t1.NS_TRANSPORTE)) AS NS_TRANSPORTE, 								
          /* NS_TOTAL */								
/*            ((SUM(t1.NS_CAPTACAO))+(SUM(t1.NS_PICKING))+(SUM(t1.NS_TRANSPORTE))) AS NS_TOTAL, 								*/
		  /* POLITICA_SERVICO */								
            (CASE								
               when (SUM(t1.PRAZO_DIAS)) <= 2 then "D+1"								
               when (SUM(t1.PRAZO_DIAS)) <= 3 then "D+2"								
               else ">D+2"								
            end								
            ) AS POLITICA_SERVICO,
          /* NS_TRANSPORTE_CAPFILIAL */								
            (SUM(t1.NS_TRANSPORTE_CAPFILIAL)) AS NS_TRANSPORTE_CAPFILIAL, 								
          /* NS_TRANSP_CAP_UTEIS */								
            sum(NS_TRANSP_CAP_UTEIS) AS NS_TRANSP_CAP_UTEIS,								
          /* NS_TOTAL_CAPFILIAL */								
/*            ((SUM(t1.NS_CAPTACAO))+(SUM(t1.NS_PICKING))+(SUM(t1.NS_TRANSPORTE_CAPFILIAL))) AS NS_TOTAL_CAPFILIAL, 								*/
          /* POLITICA_SERVICOS_CAPFILIAL */								
            (CASE								
               when (SUM(t1.PRAZO_DIAS_CAPFILIAL)) <= 2 then "D+1"								
               when (SUM(t1.PRAZO_DIAS_CAPFILIAL)) <= 3 then "D+2"								
               else ">D+2"								
            end								
            ) AS POLITICA_SERVICOS_CAPFILIAL,
          /* POLITICA_SERVICOS_CAP_UTEIS */								
            (CASE								
               when (SUM(t1.PRAZO_DIAS_CAP_UTEIS)) <= 2 then "D+1"								
               when (SUM(t1.PRAZO_DIAS_CAP_UTEIS)) <= 3 then "D+2"								
               else ">D+2"								
            end								
            ) AS POLITICA_SERVICOS_CAP_UTEIS								
      from NS_TRANSP_005 t1								
      GROUP BY t1.COD_CD, t1.CD, t1.MES, t1.ANO_MES, t1.ANO_CICLO, t1.CICLO, t1.UR, t1.COD_RE, t1.RE, t1.COD_GV, t1.GV, t1.COD_SETOR, t1.SETOR,								
               t1.UF, t1.NOME_UF, t1.CIDADE, t1.ZONEAMENTO, t1.ROTA, t1.TRANSPORTADORA, t1.FILIAL, t1.TIPO_DEM;								
QUIT;								

%mend rel_politica_servico;

%macro salva_saida_otm(ndx);
DATA SAIDA_CAP_FILIAL;
	SET SAIDA_CAP_&ndx.;
run;
DATA LOG_ALTERA_PRAZO;
	SET LOG_ALTERA_PRAZO_&ndx.;
run;
/*DATA SAIDA_DURACAO_CICLO;*/
/*	SET DURACAO_CICLO_&ndx.;*/
/*run;*/
DATA SAIDA_NS_TRANSP;
	SET BACKLOG&ndx.;
run;
data saida_ponto_a_ponto;
	set input_otm_&ndx.;
run;
%mend salva_saida_otm;
%macro append_saida_otm(ndx);

DATA SAIDA_CAP_FILIAL;
	SET SAIDA_CAP_FILIAL(where= (data < "&_data_ini."d)) SAIDA_CAP_&ndx.(where= (data >= "&_data_ini."d));
run;
DATA LOG_ALTERA_PRAZO;
	SET LOG_ALTERA_PRAZO LOG_ALTERA_PRAZO_&ndx.;
run;
/*DATA SAIDA_DURACAO_CICLO;*/
/*	SET SAIDA_DURACAO_CICLO DURACAO_CICLO_&ndx.;*/
/*run;*/
DATA SAIDA_NS_TRANSP;
	SET SAIDA_NS_TRANSP BACKLOG&ndx.;
run;	
data saida_ponto_a_ponto;
	set saida_ponto_a_ponto input_otm_&ndx.;
run;
%mend append_saida_otm;
%macro finaliza_saida_otm(ndx);

DATA LOG_ALTERA_PRAZO_&ndx.;
	SET LOG_ALTERA_PRAZO;
run;
/* Calcula a duração do ciclo*/
PROC SQL;
   CREATE TABLE SAIDA_DURACAO_CICLO_&ndx. AS 
   SELECT DISTINCT 
   		  /* UF */
            (put(t1.ZONEAMENTO,$2.)) AS UF, 
			FILIAL,
          /* ANOMES */
            (MONTH(t1.DATA_ENTREGA)*10000+YEAR(t1.DATA_ENTREGA)) AS MESANO, 
          /* SUM_of_VOLUME_ENTREGA */
            (SUM(t1.VOLUME_ENTREGA)) AS VOLUME_CICLO, 
          /* TEMPO_CICLO */
            (SUM(t1.VOLUME_ENTREGA*t1.PRAZO_ENTREGA_UTIL)/(SUM(t1.VOLUME_ENTREGA))) AS TEMPO_CICLO
      FROM WORK.SAIDA_NS_TRANSP t1
      GROUP BY (CALCULATED UF), (CALCULATED MESANO);
QUIT;
DATA SAIDA_NS_TRANSP&ndx.(DROP= TEMPO_ENTREGA);
	format TRANSPORTADORA $30. FILIAL $50. ROTA 8. ZONEAMENTO $9. TIPO_DEM $6. DATA_SAIDA_CD date. DATA_SAIDA_ENTREGA date.
	ZN_IN date. DATA_ENTREGA date. VOLUME_ENTREGA 8.2 PRAZO_ENTREGA_UTIL 8.;
	SET SAIDA_NS_TRANSP;
run;
DATA SAIDA_CAP_FILIAL&ndx.;
	SET SAIDA_CAP_FILIAL;
RUN;

data saida_ponto_a_ponto&ndx.;
	set saida_ponto_a_ponto;
run;
%mend finaliza_saida_otm;

%macro horizonte_rolante(filial,nfl);
%let dini =;
%let dfin =;
%let _exit = 0;
%let first = 1;
data _null_;
	ini = "&_data_ini."d;
	fin = "&_data_fin."d;
	call symput('_data_ini', put(ini, date.));
	call symput('_data_fin', put(fin, date.));
	call symput('dini', put(ini, date.));
	call symput('dfin', put(fin, date.));
run;
%do %while (&_exit = 0);
	data _null_;
		if "&_data_fin."d ~= "&dfin."d then do;
			ini = "&_data_fin."d + 1;
		end;
		/* Primeira rodada*/
		else do;
			ini = "&dini."d;
		end;
		fin = intnx('WEEK',ini,12);
		fin = fin - 1;
		call symput('_data_ini', put(ini,date.));
		if fin > "&dfin."d then do;
			fin = "&dfin."d;
			call symput('_data_fin',put(fin,date.));
			call symput('_exit', 1);
		end;
		else
			call symput('_data_fin',put(fin,date.));
	run;
	/* Aqui vai o código*/
	%put _data_ini = &_data_ini.;
	%put _data_fin = &_data_fin.;
	/* Primeira rodada*/
	%if "&_data_ini."d = "&dini"d %then %do;
		%otimiza(&filial.,&nfl.,1);
		%salva_saida_otm(&nfl.);
	%end;
	%else %do;
		%otimiza(&filial.,&nfl.,0);
		%append_saida_otm(&nfl.);
	%end;
	data _null_;
		/* Última rodada?*/
		if "&_data_fin."d = "&dfin."d then 
			call symput('_exit', 1);
	run;
	%if &_exit. ~= 0 %then %do;
		%finaliza_saida_otm(&nfl.);
	%end;
%end;
%let _data_ini = &dini.;
%let _data_fin = &dfin.;
%mend horizonte_rolante;


%macro main;
%preProc;
%let _filial = ;
%let n = ;
PROC SQL noprint;
   CREATE TABLE WORK.filiais AS 
   SELECT DISTINCT t1.FILIAL
      FROM SIMULA.CADASTRO_FILIAIS t1;
	select count(*) into: n from filiais;
QUIT;
PROC SQL;
	CREATE TABLE SIMULA.SAIDA_CAP_FILIAL
	(TRANSPORTADORA CHAR(30), FILIAL CHAR(50), ROTA NUM, TIPO_DEM CHAR(6), DATA DATE, MES NUM, ANO NUM,
		DEMANDA NUM, ENTREGA NUM, CAPACIDADE_UTIL NUM, CAPACIDADE_NORMAL NUM, CAPACIDADE_ESTRATEGIA NUM);

	CREATE TABLE SIMULA.LOG_ALTERA_PRAZO
	(FILIAL CHAR(50), ROTA NUM, ZONEAMENTO CHAR(9), TIPO_DEM CHAR(6), DATA_EXPEDICAO DATE, 
		DATA_ENTREGA DATE, TEMPO_ENTREGA NUM, PRAZO_ALTERADO NUM);

	CREATE TABLE SIMULA.SAIDA_DURACAO_CICLO
	(UF CHAR(2), FILIAL CHAR(50), MESANO NUM, VOLUME_CICLO NUM, TEMPO_CICLO NUM);

	CREATE TABLE SIMULA.SAIDA_NS_TRANSP
	(TRANSPORTADORA CHAR(30), FILIAL CHAR(50), ROTA NUM, ZONEAMENTO CHAR(9), TIPO_DEM CHAR(6), DATA_SAIDA_CD DATE, DATA_SAIDA_ENTREGA DATE,
	ZN_IN DATE, DATA_ENTREGA DATE, VOLUME_ENTREGA NUM, PRAZO_ENTREGA_UTIL NUM);
RUN;
DATA SIMULA.SAIDA_PONTO_A_PONTO;
	format ROTA	8. ZONEAMENTO $9. TIPO_DEM $6. TRANSPORTADORA $30. FILIAL $50. 
		VOLUME best12.  DATA_CAPTACAO date. DATA_EXPEDICAO date. DATA_HORA_EXPEDICAO datetime.
		cod_cd 8. data_hora_expedicao datetime. hub1 $30. hub1_in datetime. hub1_out datetime. 
		hub2 $30. hub2_in datetime. hub2_out datetime. hub3 $30. hub3_in datetime. hub3_out datetime. 
		filial_in datetime. filial_out datetime. 
		DATA_ENTREGA date. TEMPO_ENTREGA best12. zn_in datetime.;

RUN;

proc sql;
	delete * from SIMULA.SAIDA_PONTO_A_PONTO;
quit;

%do %while(&n. > 0);
	data filiais;
		set filiais;
		if _n_ = 1 then do;
			call symput('_filial',filial);
		end;
		else output;
	run;
	%put _filial = &_filial.;
	data _null_;
		file print;
		inicio  = time();
		put "&_filial - " inicio= time.;
		file log;
	run;
	%horizonte_rolante(&_filial., &n.);
	%let n = %eval(&n.);
	DATA SIMULA.SAIDA_CAP_FILIAL;
		SET SIMULA.SAIDA_CAP_FILIAL SAIDA_CAP_FILIAL&n.;
	run;
	DATA SIMULA.LOG_ALTERA_PRAZO;
		SET SIMULA.LOG_ALTERA_PRAZO LOG_ALTERA_PRAZO_&n.;
	run;
	DATA SIMULA.SAIDA_DURACAO_CICLO;
		SET SIMULA.SAIDA_DURACAO_CICLO SAIDA_DURACAO_CICLO_&n.;
	run;
	DATA SIMULA.SAIDA_NS_TRANSP;
		SET SIMULA.SAIDA_NS_TRANSP SAIDA_NS_TRANSP&n.;
	run;

	DATA SIMULA.SAIDA_PONTO_A_PONTO;
		SET SIMULA.SAIDA_PONTO_A_PONTO SAIDA_PONTO_A_PONTO&n.;
	run;

	proc sql noprint;
		select count(*) into: n from filiais;
	quit;
%end;

proc sort data=simula.saida_cap_filial;
	by filial rota tipo_dem data;
run;

%rel_politica_servico;
%mend main;
%main