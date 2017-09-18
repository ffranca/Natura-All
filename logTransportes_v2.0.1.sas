LIBNAME simula BASE "D:\SASCONFIG\Lev1\SASApp\Data\natura\PLANEJAMENTO_DSC";

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

%mend preProc;
%macro leDadosGerais;
/* CADASTRO_FILIAIS*/
	set<str> FilialSet;
	str transpFl{FilialSet};
	read data simula.cadastro_filiais into FilialSet=[FILIAL]
		transpFl=TRANSPORTADORA;
/*	print reFl cidadeFl tempoMovFl tpTurnoFl voloTPZN tempoEntregaFl;*/

/* ITINERARIO*/
	set<num,str> ItinerarioSet;
	str filialIti{ItinerarioSet};
	read data simula.itinerario into ItinerarioSet=[CD ZONEAMENTO]
	filialIti=FILIAL;

/* CAPACIDADE_FILIAL*/
	set<str,str> TrFilial;
	num cap_normal{TrFilial};
	num cap_estr{TrFilial};
	num entrega_sabado{TrFilial};
	num entrega_domingo{TrFilial};
	read data SIMULA.CAPACIDADE_FILIAL into TrFilial=[TRANSPORTADORA FILIAL]
		cap_normal=capacidade_normal cap_estr=capacidade_estrategia 
		entrega_sabado entrega_domingo;

/* DATA_HORA_EXP_ZN */
	set<num,num,num,num,str,str> zndiaSet;
	num volume{zndiaSet};
	num pedido{zndiaSet};
	num ns{zndiaSet};
	num tempoSep{zndiaSet};
	num tempoExp{zndiaSet};
	read data work.SAIDA_NS_ZNDIA into zndiaSet=[COD_CD DATA COD_SETOR ROTA ZONEAMENTO TIPO_DEM]
	NS VOLUME PEDIDO tempoSep=tempo_separacao tempoExp=tempo_expedicao
	;

/* Feriados*/
	set<num,num> FeriadoSet;
	str descricao{FeriadoSet};
	str tipoFeriado{FeriadoSet};
	read data simula.feriado into FeriadoSet=[COD_CD DATA] descricao tipoFeriado=TIPO;
	set Feriado = setof{<cd,data> in FeriadoSet: tipoFeriado[cd,data] = 'F'}<data>;

/* ROTA_ZONEAMENTO */
	set<num,str,num,num> RotaZone;
	num prazo_contratado{RotaZone};
	read data SIMULA.ROTA_ZONEAMENTO into RotaZone=[ROTA ZONEAMENTO DATA_INI DATA_FIN]
		prazo_contratado;



%mend leDadosGerais;


%macro log_prazos;
%preProc;
proc optmodel;

	%leDadosGerais;
/****************** O programa *************************/
	set<str,str,str,str> log init {};

/*Log 1: ERRO	CD Zomeamento sem itinerário	DATA_HORA_EXP_ZN ITINERARIO*/
	for{<cd,dt,st,rt,zn,td> in zndiaSet: <cd,zn> not in ItinerarioSet} do;
		log = log union {<'ERRO','CD (' || cd || ') ZN (' || zn || ') sem itinerário','DATA_HORA_EXP_ZN','ITINERARIO'>};
	end;
/*Log 5: ERRO	Filial inexistente	ITINERARIO	CADASTRO_FILIAIS*/
	for{<cd,zn> in ItinerarioSet: filialIti[cd,zn] ~= '-' and filialIti[cd,zn] not in FilialSet} do;
		log = log union {<'ERRO','Filial ('||filialIti[cd,zn] || ')inexistente','ITINERARIO','CADASTRO_FILIAIS'>};
	end;
/*	put log=;*/
	create data simula.log_prazos from [TIPO_LOG/length=10 DESCRICAO/length=100 TABELA1/length=30 TABELA2/length=30]=log; 
	
quit;
/* V 1.0.4 ==> Log 19: ERRO cd rota zoneamento duplicado ITINERARIO*/
PROC SQL;
   CREATE TABLE WORK.ITINERARIO19 AS 
   SELECT t1.CD AS COD_CD, 
          t1.ZONEAMENTO, 
          t1.ROTA, 
          /* CNT */
            (count(*)) AS CNT
      FROM SIMULA.ITINERARIO t1
      GROUP BY t1.CD, t1.ZONEAMENTO, t1.ROTA
      HAVING (CALCULATED CNT) > 1;
QUIT;
data ITINERARIO19(keep= TIPO_LOG DESCRICAO TABELA1 TABELA2);
	format TIPO_LOG $10. DESCRICAO $100. TABELA1 $30. TABELA2 $30.;
	set ITINERARIO19;

	TIPO_LOG = 'ERRO';
	DESCRICAO = 'CD (' || trim(INPUT(COD_CD,$20.)) || ') ROTA(' || TRIM(INPUT(ROTA,$20.)) || ') ZONEAMENTO(' || TRIM(ZONEAMENTO) || ') DUPLICADO';
	TABELA1 = 'ITINERARIO';
	TABELA2 = 'N/A';
run;
proc append base=simula.log_prazos data=itinerario19 force;
run;

/* Itinerário com duração missing*/
PROC SQL;
   CREATE TABLE WORK.ITINERARIO20 AS 
   SELECT DISTINCT 
          t1.FILIAL
      FROM simula.ITINERARIO t1
      WHERE t1.DURACAO_TOTAL = . or t1.DURACAO_CD_FILIAL = .;
QUIT;
proc sql;
	insert into simula.log_prazos select
		'ERRO' as TIPO_LOG,
		'FILIAL (' || trim(left(FILIAL)) || ') DURAÇÃO INVÁLIDA' AS DESCRICAO,
		'ITINERARIO' AS TABELA1,
		'N/A' AS TABELA2
	FROM ITINERARIO20;
quit;
/* Filial sem capacidade*/
PROC SQL;
   CREATE TABLE WORK.FILIAL_SEM_CAP AS 
   SELECT DISTINCT 
          t1.FILIAL
      FROM SIMULA.CADASTRO_FILIAIS t1
           LEFT JOIN SIMULA.CAPACIDADE_FILIAL t2 ON (t1.FILIAL = t2.FILIAL)
      WHERE t2.FILIAL = '';
	insert into simula.log_prazos 
		select 
			'ERRO' as TIPO_LOG, 
			trim(FILIAL) || ' sem capacidade'  as DESCRICAO,
			'CADASTRO_FILIAIS' AS TABELA1,
			'CAPACIDADE_FILIAL' AS TABELA2
		from FILIAL_SEM_CAP;
QUIT;
proc sql noprint;
	select count(*) into :n from simula.log_prazos;
quit;
%if &n = 0 %then %do;
	data _null_;
		file print;
		put 'Log Base de Otimização de Transportes: Sem erros.';
		file log;
	run;
%end; %else %do;
	proc sql;
		select * from simula.log_prazos;
	quit;
%end;
%mend log_prazos;
%log_prazos