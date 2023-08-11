OPTIONS VALIDVARNAME=ANY;

%macro ExtendValidMemName;

%if %sysevalf(&sysver>=9.3) %then options validmemname=extend;

%mend ExtendValidMemName;

%ExtendValidMemName;
/*libname SIMULA "E:\logical\projetos\Natura\Manutenção 2023\dados_novos";*/
/*Ambiente Antigo*/
libname SIMULA "/sasdata/DATA/NATURA/BALANCEAMENTO/PLANEJAMENTO_DSC";
/* Classe Main*/
/*%let CLASS_PATH= C:\Users\Fabio\Google Drive\Projetos\Natura\Balanceamento\Programas\v1.01;*/
/*%let CLASS_PATH=/sasdata/DATA/NATURA/BALANCEAMENTO/BALANCEAMENTO_COMPARTILHADO/REPOSITORIO_STP;*/
options mprint nosymbolgen nomlogic;
%macro init_nova_estrutura;
/* Gera tabela CD*/
PROC SQL;
   CREATE TABLE WORK.CD AS 
   SELECT DISTINCT t1.COD_CD, 
          t1.CD, 
          min(t1.DATA_INI) FORMAT=DATE. as DATA_INI, 
          max(t1.DATA_FIN) FORMAT=DATE. as DATA_FIN
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
		GROUP BY t1.COD_CD;
QUIT;
/* Criar nova tabela CD_SETOR_ZONEAMENTO*/
/* primeiro o CD*/
PROC SQL;
   CREATE TABLE WORK.CD_ZN AS 
   SELECT t1.COD_CD, 
          t1.CD, 
          t1.ZONEAMENTO, 
          /* CD_DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS CD_DATA_INI, 
          /* CD_DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS CD_DATA_FIN
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
      GROUP BY t1.COD_CD,
               t1.CD,
               t1.ZONEAMENTO;
QUIT;
/* AGORA setor*/
PROC SQL;
   CREATE TABLE WORK.SETOR_ZN AS 
   SELECT t1.COD_SETOR, 
          t1.SETOR, 
          t1.ZONEAMENTO, 
          /* SETOR_DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS SETOR_DATA_INI, 
          /* SETOR_DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS SETOR_DATA_FIN
      FROM SIMULA.ESTRUTURA_COMERCIAL t1
      GROUP BY t1.COD_SETOR,
               t1.SETOR,
               t1.ZONEAMENTO;
QUIT;
PROC SQL;
   CREATE TABLE CD_SETOR_ZONEAMENTO AS 
   SELECT DISTINCT t1.COD_CD, 
          t1.CD, 
          t2.COD_SETOR, 
          t2.SETOR, 
          t1.ZONEAMENTO, 
          MAX(t1.CD_DATA_INI, t2.SETOR_DATA_INI) FORMAT=DATE. AS DATA_INI,
          MIN(t1.CD_DATA_FIN, t2.SETOR_DATA_FIN) FORMAT=DATE. AS DATA_FIN
      FROM CD_ZN t1
           INNER JOIN SETOR_ZN t2 ON (t1.ZONEAMENTO = t2.ZONEAMENTO)
		WHERE (CALCULATED DATA_INI) < (CALCULATED DATA_FIN);
QUIT;
/* ROTA_ZONEAMENTO*/
PROC SQL;
   CREATE TABLE WORK.ROTA_ZONEAMENTO AS 
   SELECT DISTINCT t1.ROTA, 
          t1.ZONEAMENTO, 
          t1.PRAZO_CONTRATADO, 
          /* DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS DATA_INI, 
          /* DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS DATA_FIN
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
      GROUP BY t1.ROTA,
               t1.ZONEAMENTO;
QUIT;
/* ROTA_TRANSPORTADORA_FILIAL*/
PROC SQL;
   CREATE TABLE WORK.ROTA_TRANSPORTADORA_FILIAL AS 
   SELECT DISTINCT t1.ROTA, 
          t1.TRANSPORTADORA, 
          t1.FILIAL, 
          /* DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS DATA_INI, 
          /* DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS DATA_FIN
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
      GROUP BY t1.ROTA,
               t1.TRANSPORTADORA,
               t1.FILIAL;
QUIT;
/*CALENDARIZACAO*/
PROC SQL;
   CREATE TABLE WORK.CALENDARIZACAO AS 
   SELECT DISTINCT t1.ROTA, 
          t1.DOM, 
          t1.SEG, 
          t1.TER, 
          t1.QUA, 
          t1.QUI, 
          t1.SEX, 
          t1.SAB, 
          t1.HORA_CORTE, 
          t1.DIA_CORTE, 
          t1.DIA_COLETA, 
          t1.HORA_COLETA, 
          t1.STATUS, 
          t1.GRUPO, 
          /* DATA_INICIAL */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS DATA_INICIO, 
          /* DATA_FINAL */
            (MIN(t1.DATA_FIN)) FORMAT=DATE9. AS DATA_FINAL
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
      WHERE t1.DIA_CORTE NOT = .
      GROUP BY t1.ROTA,
               t1.DOM,
               t1.SEG,
               t1.TER,
               t1.QUA,
               t1.QUI,
               t1.SEX,
               t1.SAB,
               t1.HORA_CORTE,
               t1.DIA_CORTE,
               t1.DIA_COLETA,
               t1.HORA_COLETA,
               t1.STATUS,
               t1.GRUPO;
QUIT;
/* PA_ROTA*/
PROC SQL;
   CREATE TABLE WORK.PA_ROTA AS 
   SELECT t1.ROTA, 
          t1.COD_PA, 
          t1.PA, 
          /* DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS DATA_INI, 
          /* DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS DATA_FIN
      FROM SIMULA.ESTRUTURA_LOGISTICA t1
      GROUP BY t1.ROTA,
               t1.COD_PA,
               t1.PA;
QUIT;
/* ESTRUTURA_COMERCIAL*/
PROC SQL;
   CREATE TABLE WORK.zoneamento_cidade AS 
   SELECT DISTINCT t1.ZONEAMENTO, 
          t1.UF, 
          t1.NOME_UF, 
          t1.CIDADE
      FROM SIMULA.ESTRUTURA_LOGISTICA t1;
QUIT;
PROC SQL;
   CREATE TABLE WORK.ESTRUTURA_COMERCIAL AS 
   SELECT t1.UR, 
          t1.COD_RE, 
          t1.RE, 
          t1.COD_GV, 
          t1.GV, 
          t1.COD_SETOR, 
          t1.SETOR, 
          t2.UF, 
          t2.NOME_UF, 
          t2.CIDADE, 
          t1.ZONEAMENTO, 
          t1.QTD_CN, 
          t1.DATA_INI, 
          t1.DATA_FIN
      FROM SIMULA.ESTRUTURA_COMERCIAL t1
           INNER JOIN WORK.ZONEAMENTO_CIDADE t2 ON (t1.ZONEAMENTO = t2.ZONEAMENTO);
QUIT;
%mend init_nova_estrutura;

%macro leDadosCaptacao;
/******************************* CARREGA TABELAS SIMULA ***************************/
/* Carrega DEMANDA_PEDIDOS*/
	set<num,num> DemandaSet;
	set<num,num,num> DemSet;
	num demPedido{DemSet};
	read data simula.DEMANDA_PEDIDOS_RE into DemSet=[CICLO ANO COD_RE] demPedido=DEMANDA_PEDIDO;
/*	print demPedido;*/
	DemandaSet = setof{<ciclo,ano,re> in DemSet} <ciclo,ano>;
/*	put DemandaSet=;*/

/* Carrega ESTRUTURA_COMERCIAL*/
	set<num,str,num> EstrComSet;
	num reEC{EstrComSet};
	str reNomeEC{EstrComSet};
	num gvEC{EstrComSet};
	str gvNomeEC{EstrComSet};
	str setorNomeEC{EstrComSet};
	num qtdCN{EstrComSet};
	num dataFinEC{EstrComSet};
	read data WORK.ESTRUTURA_COMERCIAL into EstrComSet=[COD_SETOR ZONEAMENTO DATA_INI]
		reEC=COD_RE reNomeEC=RE gvEC=COD_GV gvNomeEC=GV setorNomeEC=SETOR qtdCN=QTD_CN dataFinEC=DATA_FIN;
/*	print qtdCN;*/

/* Carrega CD*/
	set CDSet init {};
	str CDNome{CDSet};
	num dataIniCD{CDSet};
	num dataFinCD{CDSet};
	read data CD into CDSet=[COD_CD] dataIniCD=DATA_INI CDNome=CD dataFinCD=DATA_FIN;
/*	print CDNome dataIniCD dataFinCD;*/

/* Carrega CD_SETOR_ZONEAMENTO*/
	set<num,num,str,num> CDSetorZN;
	num dataFinCS{CDSetorZN};
	read data CD_SETOR_ZONEAMENTO into CDSetorZN=[COD_CD COD_SETOR ZONEAMENTO DATA_INI] dataFinCS=DATA_FIN;
/*	print dataFinCS;*/

/* Carrega ROTA_ZONEAMENTO*/
	set<num,str,num> RotaZone;
	num dataFinRZ{RotaZone};
	read data ROTA_ZONEAMENTO into RotaZone=[ROTA ZONEAMENTO DATA_INI] dataFinRZ=DATA_FIN;
/*	print dataFinRZ;*/

/* Carrega ROTA_TRANSPORTADORA_FILIAL */
	set<str,str,num,num> RotaTransp;
	num dataFinRT{RotaTransp};
	read data ROTA_TRANSPORTADORA_FILIAL into RotaTransp=[TRANSPORTADORA FILIAL ROTA DATA_INI] dataFinRT=DATA_FIN;
/*	print dataFinRT;*/

/* Carrega CAPTACAO_DIA_SETOR*/
	num maxDiasCiclo = 35;
	set<num,num,num,num> CaptaDiaSet;
	set<num> diaCapta;
	diaCapta = {1..maxDiasCiclo};
	num percCaptaDia{CaptaDiaSet,diaCapta};
	read data simula.CAPTACAO_DIA_SETOR into CaptaDiaSet=[CICLO ANO COD_SETOR DIA_SEMANA]
	{dia in diaCapta} <percCaptaDia[ciclo,ano,cod_setor,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percCaptaDia;*/
/*	for{<cc,ano,re,gv,cv> in CaptaDiaSet}*/
/*		put percCaptaDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega CPT_PESO_DIAS_CICLO*/
	set<num> PesoDiasCicloSet;
	num percVarDiasCiclo{PesoDiasCicloSet};
	read data simula.CPT_PESO_DIAS_CICLO into PesoDiasCicloSet=[DIAS_CICLO] percVarDiasCiclo=VARIACAO_DEMANDA;
/*	print percVarDiasCiclo;*/

/* Carrega RELACAO_DIA_SETOR*/
	set<num,num,num,num> RelDiaSet;
	set<num> diaRel;
	diaRel = {1..maxDiasCiclo};
	num percRelDia{RelDiaSet,diaRel};
	read data simula.RELACAO_DIA_SETOR into RelDiaSet=[CICLO ANO COD_SETOR DIA_SEMANA]
	{dia in diaRel} <percRelDia[ciclo,ano,cod_setor,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percRelDia;*/
/*	for{<cc,ano,re,gv,cv> in RelDiaSet}*/
/*		put percRelDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega RELACAO_ITEM_DIA_SETOR*/
	set<num,num,num,num> RelItemDiaSet;
	num percRelItemDia{RelItemDiaSet,diaRel};
	read data simula.RELACAO_ITEM_DIA_SETOR into RelItemDiaSet=[CICLO ANO COD_SETOR DIA_SEMANA]
	{dia in diaRel} <percRelItemDia[ciclo,ano,cod_setor,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percRelDia;*/
/*	for{<cc,ano,re,gv,cv> in RelItemDiaSet}*/
/*		put percRelDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega CALENDARIO_SETOR*/
	set<num,num,num> Calendario;
	num abreCiclo{Calendario};
	num abreCicloOrg{Calendario};
	num fechaCiclo{Calendario};
	str estrategiaCal{Calendario};
	read data simula.CALENDARIO_SETOR into Calendario=[CICLO ANO COD_SETOR]
		abreCiclo=ABERTURA abreCicloOrg=ABERTURA fechaCiclo=FECHAMENTO estrategiaCal=ESTRATEGIA;
/*	print abreCiclo fechaCiclo;*/

/* Carrega CALENDARIZACAO*/
	set<num,num> CalendSet;
	num horaCorteCAL{CalendSet};
	num diaCorteCAL{CalendSet};
	num horaColetaCAL{CalendSet};
	num diaColetaCAL{CalendSet};
	num dataFinCAL{CalendSet};
	str DOW{1..7} = ['DOMINGO' 'SEGUNDA' 'TERCA' 'QUARTA' 'QUINTA' 'SEXTA' 'SABADO'];
	str tempCalendDOW{CalendSet,1..7};
	num calendDOW{CalendSet,1..7} init 0;
	read data CALENDARIZACAO into CalendSet=[ROTA DATA_INICIO] 
		horaCorteCAL=HORA_CORTE diaCorteCAL=DIA_CORTE dataFinCAL=DATA_FINAL
		diaColetaCAL=DIA_COLETA horaColetaCAL=HORA_COLETA
		{i in 1..7} <tempCalendDOW[rota,data_inicio,i] = col(substr(DOW[i],1,3))>;
	for{<z,di> in CalendSet, d in 1..7}
		if tempCalendDOW[z,di,d] ~= '' then calendDOW[z,di,d] = 1;
/*	print calendDOW;*/

/* Carrega CAPTACAO_DIA_CC_SETOR*/
	set<num,num,num,num,num> CaptaDiaCCSet;
	set diaCaptaCC = {-20..-1};
	num percCaptaCiclo{CaptaDiaCCSet};
	num percCaptaDiaCC{CaptaDiaCCSet,diaCaptaCC};
	num pedCC{CaptaDiaCCSet,diaCaptaCC} init 0;
	read data simula.CAPTACAO_DIA_CC_SETOR into CaptaDiaCCSet=[CICLO ANO COD_SETOR DIA_SEMANA DATA_EVENTO] percCaptaCiclo=perc_ciclo
	{dia in diaCaptaCC} <percCaptaDiaCC[ciclo,ano,cod_setor,dia_semana,data_evento,dia] = col(compress("I" || put(dia,8.0)))>;
/*	print percCaptaDiaCC;*/
/*	for{<cc,ano,re,gv,cv> in CaptaDiaCCSet}*/
/*		put percCaptaDiaCC[cc,ano,re,gv,cv,-1]=;*/

/* Carrega RELACAO_DIA_CC_SETOR*/
	set<num,num,num,num> RelDiaCCSet;
	set diaRelCC = {-20..-1};
	num percRelDiaCC{RelDiaCCSet,diaRelCC};
	read data simula.RELACAO_DIA_CC_SETOR into RelDiaCCSet=[CICLO ANO COD_SETOR DIA_SEMANA]
	{dia in diaRelCC} <percRelDiaCC[ciclo,ano,cod_setor,dia_semana,dia] = col(compress("I" || put(dia,8.0)))>;
/*	print percRelDiaCC;*/
/*	for{<cc,ano,re,gv,cv> in RelDiaCCSet}*/
/*		put percRelDiaCC[cc,ano,re,gv,cv,-1]=;*/

/* Carrega RELACAO_ITEM_DIA_CC_SETOR*/
	set<num,num,num,num> RelItemDiaCCSet;
	num percRelItemDiaCC{RelItemDiaCCSet,diaRelCC};
	read data simula.RELACAO_ITEM_DIA_CC_SETOR into RelItemDiaCCSet=[CICLO ANO COD_SETOR DIA_SEMANA]
	{dia in diaRelCC} <percRelItemDiaCC[ciclo,ano,cod_setor,dia_semana,dia] = col(compress("I" || put(dia,8.0)))>;
/*	print percRelItemDiaCC;*/
/*	for{<cc,ano,re,gv,cv> in RelItemDiaCCSet}*/
/*		put percRelItemDiaCC[cc,ano,re,gv,cv,-1]=;*/

/* Carrega FERIADO*/
	set<num,num> Feriado;
	num folgaNoturno{Feriado};
	read data simula.FERIADO(where=(tipo='F')) into Feriado=[COD_CD DATA] folgaNoturno=FOLGA_NOTURNO;
/*	print folgaNoturno tipoFeriado;*/

%mend leDadosCaptacao;

%macro log_estrutura_com;
/*Log 1: ESTRUTURA_COMERCIAL	AVISO	Setor/zoneamento sem cadastro de xx/xx/xx a xx/xx/xx */
/*Log 2: ESTRUTURA_COMERCIAL	AVISO	Setor/zoneameto com cadastro duplicado entre xx/xx/xx e xx/xx/xx*/
proc sort data=WORK.ESTRUTURA_COMERCIAL;
	by cod_setor zoneamento data_ini;
run;

data log1e2;
	set WORK.ESTRUTURA_COMERCIAL;
	by cod_setor zoneamento;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.zoneamento then
		last_fin = data_fin;
	else do;
		if data_ini - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if data_ini <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = data_fin;
	end;
run;

proc sql;
	create table log1e2_001 as select * from log1e2
	where gap_error = 1;
		
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			trim(setor) || '/' || trim(zoneamento) || ' sem cadastro entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_001;
quit;
proc sql;
	create table log1e2_002 as select * from log1e2
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			trim(setor) || '/' || trim(zoneamento) || ' com cadastro duplicado entre ' || 
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_002;
quit;
%mend log_estrutura_com;
%macro log_cd_setor;
/*Log 20: CD_SETOR	CD	ERRO	CD inexistente*/
PROC SQL;
   CREATE TABLE WORK.LOG20_001 AS 
   SELECT t1.COD_SETOR, 
          t1.SETOR, 
          t1.COD_CD, 
          t1.CD, 
          t1.DATA_INI, 
          t1.DATA_FIN
      FROM WORK.CD_SETOR_ZONEAMENTO t1 LEFT JOIN WORK.CD t2 ON (t1.COD_CD = t2.COD_CD)
      WHERE t2.COD_CD IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			trim(setor) || ' com CD inexistente(' || compress(put(cod_cd,8.)) || ') entre ' || 
				put(data_ini, date9.) || ' e ' || put(data_fin, date9.) as DESCRICAO,
			'CD_SETOR_ZONEAMENTO' AS TABELA1,
			'CD' AS TABELA2
		from WORK.LOG20_001;
quit;

/*Log 21: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor inexistente*/
PROC SQL;
   CREATE TABLE WORK.LOG21_001 AS 
   SELECT DISTINCT t1.COD_SETOR, 
          t1.SETOR, 
          t1.COD_CD, 
          t1.CD, 
          t1.DATA_INI, 
          t1.DATA_FIN
      FROM WORK.CD_SETOR_ZONEAMENTO t1 LEFT JOIN WORK.ESTRUTURA_COMERCIAL t2 ON (t1.COD_SETOR = t2.COD_SETOR)
      WHERE t2.COD_SETOR IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'Código do setor ' || trim(setor) || ' inexistente(' || compress(put(cod_setor,8.)) || 
				') entre ' || put(data_ini, date9.) || ' e ' || put(data_fin, date9.) as DESCRICAO,
			'CD_SETOR_ZONEAMENTO' AS TABELA1,
			'ESTRUTURA_COMERCIAL' AS TABELA2
		from WORK.LOG21_001;
quit;

/*Log 22a: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor sem separação entre xx/xx/xx e xx/xx/xx*/
/*Log 22b: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor separado em mais de um CD entre xx/xx/xx e xx/xx/xx*/
proc sort data=WORK.CD_SETOR_ZONEAMENTO;
	by cod_setor zoneamento data_ini;
run;

data log22ab_001;
	set WORK.CD_SETOR_ZONEAMENTO;
	by cod_setor zoneamento;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.zoneamento then
		last_fin = data_fin;
	else do;
		if data_ini - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if data_ini <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = data_fin;
	end;
run;

proc sql;
	create table log22ab_002 as select * from log22ab_001
	where gap_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'Zoneamento ' || compress(put(cod_setor,8.)) || ' - ' || trim(zoneamento) || ' sem separação entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			'CD_SETOR_ZONEAMENTO' AS TABELA1,
			'N/A' AS TABELA2
		from log22ab_002;
quit;
proc sql;
	create table log22ab_003 as select * from log22ab_001
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'Zoneamento ' || compress(put(cod_setor,8.)) || ' - ' || trim(zoneamento) || ' com cadastro duplicado entre ' || 
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'CD_SETOR_ZONEAMENTO' AS TABELA1,
			'N/A' AS TABELA2
		from log22ab_003;
quit;
%mend log_cd_setor;
%macro log_calendario;
/*Log 11A: ESTRUTURA_COMERCIAL	CALENDARIO	ERRO	 SETOR sem calendário*/
PROC SQL;
   CREATE TABLE WORK.LOG11A_001 AS 
   SELECT t1.COD_SETOR, 
          t1.SETOR
      FROM WORK.ESTRUTURA_COMERCIAL t1 LEFT JOIN SIMULA.CALENDARIO_SETOR t2 ON (t1.COD_SETOR = t2.COD_SETOR)
      WHERE t2.COD_SETOR IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || ') sem calendário' as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'CALENDARIO_SETOR' AS TABELA2
		from WORK.LOG11A_001;
quit;
/*Log 11B: CALENDARIO	CALENDARIO	ERRO	 SETOR sem ciclo entre xx/xx/xx e xx/xx/xx*/
proc sort data=simula.calendario_setor;
	by cod_setor abertura;
run;

data log11b_001;
	set simula.calendario_setor;
	by cod_setor;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.cod_setor then
		last_fin = fechamento;
	else do;
		if abertura - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if abertura <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = fechamento;
	end;
run;

proc sql;
	create table log11b_002 as select * from log11b_001
	where gap_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || ') sem ciclo entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(abertura-1, date9.) as DESCRICAO,
			'CALENDARIO_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from log11b_002;
quit;
/*Log 12: CALENDARIO	CALENDARIO	ERRO	SETOR com mais de um ciclo entre xx/xx/xx e xx/xx/xx*/
proc sql;
	create table log11b_003 as select * from log11b_001
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || ') com mais de um ciclo entre ' || 
				put(abertura, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'CALENDARIO_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from log11b_003;
quit;
%mend log_calendario;

%macro log_calendarizacao;
/*Log 7: CALENDARIZAÇÃO	PA_ROTA	AVISO	Rota inexistente*/
PROC SQL;
   CREATE TABLE WORK.LOG7_001 AS 
   SELECT t1.*
      FROM WORK.CALENDARIZACAO t1 LEFT JOIN WORK.PA_ROTA t2 ON (t1.ROTA = t2.ROTA)
      WHERE t2.ROTA IS MISSING AND t1.ROTA NOT = 0;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(ROTA,8.)) || ' inexistente' as DESCRICAO,
			'CALENDARIZACAO' AS TABELA1,
			'PA_ROTA' AS TABELA2
		from WORK.LOG7_001;
quit;
/*Log 8: CALENDARIZAÇÃO	CALENDARIZAÇÃO	AVISO	Rota sem dia de calendarização*/
PROC SQL;
   CREATE TABLE WORK.LOG8_001 AS 
   SELECT t1.ROTA
      FROM WORK.CALENDARIZACAO t1
      WHERE t1.DOM IS MISSING AND t1.SEG IS MISSING AND t1.TER IS MISSING AND t1.QUA IS MISSING AND t1.QUI IS MISSING AND
            t1.SEX IS MISSING AND t1.SAB IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(ROTA,8.)) || ' sem dia de calendarização' as DESCRICAO,
			'ESTRUTURA_LOGISTICA' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG8_001;
quit;
/*Log 9: CALENDARIZAÇÃO	CALENDARIZAÇÃO	AVISO	Dia de corte inválido*/
PROC SQL;
   CREATE TABLE WORK.LOG9_001 AS 
   SELECT t1.ROTA,t1.DIA_CORTE
      FROM WORK.CALENDARIZACAO t1
      WHERE t1.DIA_CORTE NOT BETWEEN -1 AND 0;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(ROTA,8.)) || ' com dia de corte inválido (' 
				||  compress(put(DIA_CORTE,8.)) || ')' as DESCRICAO,
			'CALENDARIZACAO' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG9_001;
quit;
/*Log 10: CALENDARIZAÇÃO	CALENDARIZAÇÃO	AVISO	Dia de coleta inválido*/
PROC SQL;
   CREATE TABLE WORK.LOG10_001 AS 
   SELECT t1.ROTA,t1.DIA_COleta
      FROM WORK.CALENDARIZACAO t1
      WHERE t1.DIA_COLETA NOT BETWEEN 0 AND 1;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(ROTA,8.)) || ' com dia de coleta inválido (' 
				||  compress(put(DIA_COLETA,8.)) || ')' as DESCRICAO,
			'CALENDARIZACAO' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG10_001;
quit;

%mend log_calendarizacao;

%macro log_captacao_dia;
/*Log 13: CAPTACAO_DIA	ESTRUTURA_COMERCIAL	AVISO	SETOR sem captação dia*/
PROC SQL;
   CREATE TABLE WORK.LOG13_001 AS 
   SELECT DISTINCT t1.COD_SETOR, 
          t1.SETOR
      FROM WORK.ESTRUTURA_COMERCIAL t1 LEFT JOIN SIMULA.CAPTACAO_DIA_SETOR t2 ON (t1.COD_SETOR = t2.COD_SETOR)
      WHERE t2.COD_SETOR IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || ') sem captação dia' as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'CAPTACAO_DIA_SETOR' AS TABELA2
		from WORK.LOG13_001;
quit;
/*Log 14a: CAPTACAO_DIA	CALENDARIO	AVISO	Captação com número de dias de ciclo diferente do CALENDARIO*/
PROC SQL;
   CREATE TABLE WORK.LOG14A_001 AS 
   SELECT DISTINCT t1.CICLO, 
          t1.ANO, 
          t1.COD_SETOR, 
          t1.SETOR, 
          t1.DIA_SEMANA, 
          t1.'1'n as d1, 
          t1.'2'n as d2, 
          t1.'3'n as d3, 

          t1.'4'n as d4, 
          t1.'5'n as d5, 
          t1.'6'n as d6, 
          t1.'7'n as d7, 
          t1.'8'n as d8, 
          t1.'9'n as d9, 
          t1.'10'n as d10, 
          t1.'11'n as d11, 
          t1.'12'n as d12, 
          t1.'13'n as d13, 
          t1.'14'n as d14, 
          t1.'15'n as d15, 
          t1.'16'n as d16, 
          t1.'17'n as d17, 
          t1.'18'n as d18, 
          t1.'19'n as d19, 
          t1.'20'n as d20, 
          t1.'21'n as d21, 
          t1.'22'n as d22, 
          t1.'23'n as d23, 
          t1.'24'n as d24, 
          t1.'25'n as d25, 
          t1.'26'n as d26, 
          t1.'27'n as d27, 
          t1.'28'n as d28, 
          t1.'29'n as d29, 
          t1.'30'n as d30, 
          t1.'31'n as d31, 
          t1.'32'n as d32, 
          t1.'33'n as d33, 
          t1.'34'n as d34, 
          t1.'35'n as d35, 
          t2.ABERTURA, 
          t2.FECHAMENTO
      FROM SIMULA.CAPTACAO_DIA_SETOR t1, SIMULA.CALENDARIO_SETOR t2
      WHERE (t1.COD_SETOR = t2.COD_SETOR AND t1.CICLO = t2.CICLO AND t1.ANO = t2.ANO AND
            (weekday(t2.ABERTURA)=t1.DIA_SEMANA));
QUIT;
data LOG14A_002;
	set LOG14A_001;
	array dia(35) d1-d35;
	count = 0;
	do i=1 to 35;
		if dia[i] ~= 0 then count = count + 1;
	end;
	if count ~= (fechamento - abertura + 1) then output;
run;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - captação com número de dias de ciclo diferente do CALENDARIO' as DESCRICAO,
			'CAPTACAO_DIA_SETOR' AS TABELA1,
			'CALENDARIO_SETOR' AS TABELA2
		from WORK.LOG14A_002;
quit;
/*Log 14b: CAPTACAO_DIA	CALENDARIO	AVISO	SETOR sem captação para o ano ciclo*/
PROC SQL;
   CREATE TABLE WORK.LOG14B_001 AS 
   SELECT DISTINCT t2.COD_SETOR, 
          t2.SETOR, 
          t2.CICLO, 
          t2.ANO, 
          t2.ESTRATEGIA, 
          t2.ABERTURA, 
          t2.FECHAMENTO
      FROM SIMULA.CAPTACAO_DIA_SETOR t1 RIGHT JOIN SIMULA.CALENDARIO_SETOR t2 ON (t1.COD_SETOR = t2.COD_SETOR)
           AND (t1.CICLO = t2.CICLO) AND (t1.ANO = t2.ANO AND (weekday(t2.ABERTURA)=t1.DIA_SEMANA))
      WHERE t1.COD_SETOR IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - sem captação dia' as DESCRICAO,
			'CAPTACAO_DIA_SETOR' AS TABELA1,
			'CALENDARIO_SETOR' AS TABELA2
		from WORK.LOG14B_001;
quit;
/*Log 15: CAPTACAO_DIA	CAPTACAO_DIA	ERRO	Dia da semana inválido*/
PROC SQL;
   CREATE TABLE WORK.LOG15_001 AS 
   SELECT t1.COD_SETOR, 
          t1.SETOR, 
          t1.DIA_SEMANA
      FROM SIMULA.CAPTACAO_DIA_SETOR t1
      WHERE t1.DIA_SEMANA NOT BETWEEN 1 AND 7;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || 
				') - dia da semana inválido(' || compress(put(dia_semana,8.)) || ')' as DESCRICAO,
			'CAPTACAO_DIA_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG15_001;
quit;
/*Log 16: CAPTACAO_DIA	CAPTACAO_DIA	ERRO	Captação total dierente de 100%*/
data LOG16_001;
	set LOG14A_001;
	array dia(35) d1-d35;
	total = 0;
	do i=1 to 35;
		total = total + dia[i];
	end;
	if abs(total - 1) >= 0.01 then output;
run;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'SETOR ' || trim(setor) || '(' || compress(put(cod_setor,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - captação total diferente de 100%(' || compress(put(total,percent20.2)) || ')' as DESCRICAO,
			'CAPTACAO_DIA_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG16_001;
quit;
%mend log_captacao_dia;
%macro log_rota_transp_filial;
/*AVISO	Rota sem transportadora de xx/xx/xx a xx/xx/xx*/
proc sort data=work.rota_transportadora_filial;
	by rota data_ini;
run;

data log1e2;
	set work.rota_transportadora_filial;
	by rota;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.rota then
		last_fin = data_fin;
	else do;
		if data_ini - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if data_ini <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = data_fin;
	end;
run;

proc sql;
	create table log1e2_001 as select * from log1e2
	where gap_error = 1;
quit;
proc sql;	
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(rota,8.)) || ' sem cadastro entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			'ROTA_TRANSPORTADORA_FILIAL' AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_001;
quit;
proc sql;
	create table log1e2_002 as select * from log1e2
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(rota,8.)) || ' com cadastro duplicado entre ' || 
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'ROTA_TRANSPORTADORA_FILIAL' AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_002;
quit;

%mend log_rota_transp_filial;

%macro verifica_tempo(tabela,ord1,tipo1);
proc sort data=work.&tabela.;
	by &ord1. data_ini;
run;

data log1e2;
	set work.&tabela.;
	by &ord1. ;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.&ord1. then
		last_fin = data_fin;
	else do;
		if data_ini - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if data_ini <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = data_fin;
	end;
run;

proc sql;
	create table log1e2_001 as select * from log1e2
	where gap_error = 1;
quit;
proc sql;	
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1. = CHAR %then %do;
				"&ord1. " || compress(&ord1.) || ' sem cadastro entre ' || 
			%end; %else %do;
				"&ord1. " || compress(put(&ord1.,8.)) || ' sem cadastro entre ' || 
			%end;
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			"&tabela." AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_001;
quit;
proc sql;
	create table log1e2_002 as select * from log1e2
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1. = CHAR %then %do;
				"&ord1. " || compress(&ord1.) || ' com cadastro duplicado entre ' || 
			%end; %else %do;
				"&ord1. " || compress(put(&ord1.,8.)) || ' com cadastro duplicado entre ' || 
			%end;
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			"&tabela." AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_002;
quit;
%mend verifica_tempo;
%macro verifica_tempo_chv3(tabela,ord1,tipo1,ord2,tipo2,ord3,tipo3);
proc sort data=simula.&tabela.;
	by &ord1. &ord2. &ord3. data_ini;
run;

data log1e2;
	set simula.&tabela.;
	by &ord1. &ord2. &ord3.;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.&ord3. then
		last_fin = data_fin;
	else do;
		if data_ini - last_fin > 1 then do;
			gap_error = 1;
			ini_gap = last_fin;
		end;
		if data_ini <= last_fin then do;
			ovl_error = 1;
			ini_ovl = last_fin;
		end;
		last_fin = data_fin;
	end;
run;

proc sql;
	create table log1e2_001 as select * from log1e2
	where gap_error = 1;
quit;
proc sql;	
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1. = CHAR %then %do;
				"&ord1. " || compress(&ord1.) || 
			%end; %else %do;
				"&ord1. " || compress(put(&ord1.,8.)) ||
			%end;
			%if &tipo2. = CHAR %then %do;
				" &ord2. " || compress(&ord2.) || 
			%end; %else %do;
				" &ord2. " || compress(put(&ord2.,8.)) ||
			%end;
			%if &tipo3. = CHAR %then %do;
				" &ord3. " || compress(&ord3.) || 
			%end; %else %do;
				" &ord3. " || compress(put(&ord3.,8.)) ||
			%end;
			' sem cadastro entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			"&tabela." AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_001;
quit;
proc sql;
	create table log1e2_002 as select * from log1e2
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1. = CHAR %then %do;
				"&ord1. " || compress(&ord1.) || 
			%end; %else %do;
				"&ord1. " || compress(put(&ord1.,8.)) ||
			%end;
			%if &tipo2. = CHAR %then %do;
				" &ord2. " || compress(&ord2.) || 
			%end; %else %do;
				" &ord2. " || compress(put(&ord2.,8.)) ||
			%end;
			%if &tipo3. = CHAR %then %do;
				" &ord3. " || compress(&ord3.) || 
			%end; %else %do;
				" &ord3. " || compress(put(&ord3.,8.)) ||
			%end;
			' com cadastro duplicado entre ' || 
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			"&tabela." AS TABELA1,
			'N/A' AS TABELA2
		from log1e2_002;
quit;
%mend verifica_tempo_chv3;
%macro verifica_cd(tabela);
/*AVISO	CD inexistente*/
PROC SQL;

   CREATE TABLE WORK.log_cd_001 AS 
   SELECT t1.COD_CD
      FROM SIMULA.&tabela. t1 LEFT JOIN WORK.CD t2 ON (t1.COD_CD = t2.COD_CD)
      WHERE t2.COD_CD IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'CD ' || compress(put(cod_cd,8.)) || ' inexistente'  as DESCRICAO,
			"&tabela." AS TABELA1,
			'CD' AS TABELA2
		from WORK.log_cd_001;
quit;

/*AVISO	CD sem cadastro de tempo de separação*/
PROC SQL;
   CREATE TABLE WORK.LOG_cd_002 AS 
   SELECT t2.COD_CD, 
          t2.CD
      FROM WORK.CD t2 LEFT JOIN SIMULA.&tabela. t1 ON (t2.COD_CD = t1.COD_CD)
      WHERE t1.COD_CD IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'CD ' || compress(put(cod_cd,8.)) || ' ' || trim(cd) || ' sem informações'  as DESCRICAO,
			"&tabela." AS TABELA1,
			'CD' AS TABELA2
		from WORK.LOG_cd_002;
quit;
%mend verifica_cd;
%macro verifica_chave0(tabela1,chave1,tipo1,chave2);
/*AVISO	chave inexistente*/
PROC SQL;
   CREATE TABLE WORK.log_chv_001 AS 
   SELECT DISTINCT t1.&chave1.
      FROM work.&tabela1. t1 
      WHERE t1.&chave2. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1 = CHAR %then %do;
				"&chave1. " || compress(&chave1.) || " &chave2. " || ' nula'  as DESCRICAO,
			%end;
			%else %do;
				"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || ' nula'  as DESCRICAO,
			%end;
			"&tabela1." AS TABELA1,
			"N/A" AS TABELA2
		from WORK.log_chv_001;
quit;
%mend verifica_chave0;
%macro verifica_chave1(lib1,tabela1,lib2,tabela2,chave1,tipo);
/*AVISO	chave inexistente*/
PROC SQL;
   CREATE TABLE WORK.log_chv_001 AS 
   SELECT DISTINCT t1.&chave1.
      FROM &lib1..&tabela1. t1 LEFT JOIN &lib2..&tabela2. t2 ON t1.&chave1.=t2.&chave1.
      WHERE t2.&chave1. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo = CHAR %then %do;
				"&chave1. " || compress(&chave1.) || ' inexistente'  as DESCRICAO,
			%end;
			%else %do;
				"&chave1. " || compress(put(&chave1.,8.)) || ' inexistente'  as DESCRICAO,
			%end;
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.log_chv_001;
quit;

/*AVISO	chave sem cadastro de tempo de separação*/
PROC SQL;
   CREATE TABLE WORK.LOG_chv_002 AS 
   SELECT t2.&chave1.
      FROM &lib2..&tabela2. t2 LEFT JOIN &lib1..&tabela1. t1 ON (t1.&chave1.=t2.&chave1.)
      WHERE t1.&chave1. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo = CHAR %then %do;
				"&chave1. " || compress(&chave1.) || ' sem informações'  as DESCRICAO,
			%end;
			%else %do;
				"&chave1. " || compress(put(&chave1.,8.)) || ' sem informações'  as DESCRICAO,
			%end;
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.LOG_chv_002;
quit;
%mend verifica_chave1;
%macro verifica_chave2(tabela1,tabela2,chave1,chave2,tipo);
/*AVISO	chave inexistente*/
PROC SQL;
   CREATE TABLE WORK.log_chv_001 AS 
   SELECT DISTINCT t1.&chave1.,
		t1.&chave2.
      FROM SIMULA.&tabela1. t1 LEFT JOIN SIMULA.&tabela2. t2 ON t1.&chave1.=t2.&chave1. and t1.&chave2.=t2.&chave2.
      WHERE t2.&chave2. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			"&tipo." as TIPO, 
			"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || compress(put(&chave2.,8.)) || ' inexistente'  as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.log_chv_001;
quit;

/*AVISO	chave sem cadastro de tempo de separação*/
PROC SQL;
   CREATE TABLE WORK.LOG_chv_002 AS 
   SELECT t2.&chave1.,
   		t2.&chave2.
      FROM SIMULA.&tabela2. t2 LEFT JOIN SIMULA.&tabela1. t1 ON (t1.&chave1.=t2.&chave1. and t1.&chave2.=t2.&chave2.)
      WHERE t1.&chave2. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			"&tipo." as TIPO, 
			"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || compress(put(&chave2.,8.)) || ' sem informações'  as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.LOG_chv_002;
quit;
%mend verifica_chave2;
%macro verifica_chave3(tabela1,tabela2,chave1,chave2,chave3);
/*AVISO	chave inexistente*/
PROC SQL;
   CREATE TABLE WORK.log_chv_001 AS 
   SELECT DISTINCT t1.&chave1.,
		t1.&chave2.,
		t1.&chave3.
      FROM SIMULA.&tabela1. t1 LEFT JOIN SIMULA.&tabela2. t2 ON t1.&chave1.=t2.&chave1. and t1.&chave2.=t2.&chave2. and t1.&chave3.=t2.&chave3.
      WHERE t2.&chave3. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || compress(put(&chave2.,8.)) || " &chave3. " || compress(put(&chave3.,8.)) || ' inexistente'  as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.log_chv_001;
quit;

/*AVISO	chave sem cadastro de tempo de separação*/
PROC SQL;
   CREATE TABLE WORK.LOG_chv_002 AS 
   SELECT t2.&chave1.,
   		t2.&chave2.,
		t2.&chave3.
      FROM SIMULA.&tabela2. t2 LEFT JOIN SIMULA.&tabela1. t1 ON (t1.&chave1.=t2.&chave1. and t1.&chave2.=t2.&chave2. and t1.&chave3.=t2.&chave3.)
      WHERE t1.&chave3. IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || compress(put(&chave2.,8.)) ||" &chave3. " || compress(put(&chave3.,8.)) || ' sem informações'  as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from WORK.LOG_chv_002;
quit;
%mend verifica_chave3;
%macro verifica_tempo1(tabela1,tabela2,chave1,tipo1);
PROC SQL;
   CREATE TABLE WORK.log_tmp2_001 AS 
   SELECT t1.&chave1., 
          /* MIN_of_DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS MIN_of_DATA_INI, 
          /* MAX_of_DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS MAX_of_DATA_FIN
      FROM work.&tabela1. t1
      GROUP BY t1.&chave1.;
QUIT;

PROC SQL;
   CREATE TABLE WORK.LOG_TMP2_002 AS 
   SELECT distinct t2.&chave1., 
          t2.DATA_INI, 
          t2.DATA_FIN, 
          t1.MIN_of_DATA_INI, 
          t1.MAX_of_DATA_FIN
      FROM WORK.LOG_TMP2_001 t1, work.&tabela2. t2
      WHERE (t1.&chave1. = t2.&chave1.) order by t1.&chave1., t2.data_ini;
QUIT;
data log_tmp2_003;
	set log_tmp2_002;
	format gap_ini date9. gap_fin date9.;

	if data_ini < min_of_data_ini then do;
		gap_ini = data_ini;
		gap_fin = min_of_data_ini-1;
		output;
	end;
	if data_fin > max_of_data_fin then do;
		gap_ini = max_of_data_fin+1;
		gap_fin = data_fin;
		output;
	end;
run;
proc sql;	
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			%if &tipo1. = CHAR %then %do;
				"&chave1. " || compress(&chave1.) || 
			%end; %else %do;
				"&chave1. " || compress(put(&chave1.,8.)) || 
			%end;
			' sem cadastro entre ' || put(gap_ini, date9.) || ' e ' || put(gap_fin, date9.) as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from log_tmp2_003;
quit;
%mend verifica_tempo1;
%macro verifica_tempo2(tabela1,tabela2,chave1,chave2);
PROC SQL;
   CREATE TABLE WORK.log_tmp2_001 AS 
   SELECT t1.&chave1., 
          t1.&chave2., 
          /* MIN_of_DATA_INI */
            (MIN(t1.DATA_INI)) FORMAT=DATE9. AS MIN_of_DATA_INI, 
          /* MAX_of_DATA_FIN */
            (MAX(t1.DATA_FIN)) FORMAT=DATE9. AS MAX_of_DATA_FIN
      FROM SIMULA.&tabela1. t1
      GROUP BY t1.&chave1., t1.&chave2.;
QUIT;

PROC SQL;
   CREATE TABLE WORK.LOG_TMP2_002 AS 
   SELECT t2.&chave1., 
          t2.&chave2., 
          t2.DATA_INI, 
          t2.DATA_FIN, 
          t1.MIN_of_DATA_INI, 
          t1.MAX_of_DATA_FIN
      FROM WORK.LOG_TMP2_001 t1, SIMULA.&tabela2. t2
      WHERE (t1.&chave1. = t2.&chave1. AND t1.&chave2. = t2.&chave2.);
QUIT;
data log_tmp2_003;
	set log_tmp2_002;
	format gap_ini date9. gap_fin date9.;

	if data_ini < min_of_data_ini then do;
		gap_ini = data_ini;
		gap_fin = min_of_data_ini-1;
		output;
	end;
	if data_fin > max_of_data_fin then do;
		gap_ini = max_of_data_fin+1;
		gap_fin = data_fin;
		output;
	end;
run;
proc sql;	
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			"&chave1. " || compress(put(&chave1.,8.)) || " &chave2. " || compress(put(&chave2.,8.)) || 
			' sem cadastro entre ' || put(gap_ini, date9.) || ' e ' || put(gap_fin, date9.) as DESCRICAO,
			"&tabela1." AS TABELA1,
			"&tabela2." AS TABELA2
		from log_tmp2_003;
quit;
%mend verifica_tempo2;

%macro log_relacao_dia;
%verifica_chave1(simula,RELACAO_DIA_SETOR,work,ESTRUTURA_COMERCIAL, COD_SETOR, AVISO);
%mend log_relacao_dia;
%macro log_rota_zoneamento;
%verifica_chave1(work,ROTA_ZONEAMENTO,work,PA_ROTA,rota);
%verifica_chave1(work,ESTRUTURA_COMERCIAL,work,ROTA_ZONEAMENTO,zoneamento,CHAR);
%verifica_chave0(ROTA_ZONEAMENTO,zoneamento,CHAR,rota);
%verifica_chave0(ROTA_ZONEAMENTO,rota,NUM,zoneamento);
%verifica_tempo(ROTA_ZONEAMENTO,zoneamento,CHAR);
%verifica_tempo1(ESTRUTURA_COMERCIAL,ROTA_ZONEAMENTO,zoneamento,CHAR);
%mend log_rota_zoneamento;
%macro log_demanda;
%verifica_chave2(DEMANDA_PEDIDOS_RE,CALENDARIO_SETOR,CICLO,ANO,ERRO);

%mend log_demanda;
%macro LOG_Insere(tipo, desc, tab1, tab2);
	proc sql;
		insert into simula.log 
			set tipo="&tipo", descricao="&desc", tabela1="&tab1", tabela2="&tab2";
	quit;
%mend LOG_Insere;
%macro LOG_InsereTabela(tab_erros);
	proc sql;
		insert into simula.log 
			select * from &tab_erros;
	quit;
%mend LOG_InsereTabela;
%macro log_verificaTabela(lib,nome,rc,padrao);
	%let existe = 0;
	proc sql noprint;
	select count(memname) into :existe
	   from dictionary.tables
	   where libname="&lib" and memname="&nome";
	quit;
	%if &existe = 0 %then %do;
		%LOG_Insere(ERRO, Tabela não encontrada, &nome, N/A)
		%let &rc = 1;
	%end;
	/* Trata colunas de acordo com tabela padrão*/
	%if &padrao. ne and &&&rc = 0 %then %do;
		/* Verifica colunas*/
		proc sql;
			create table str_Tabela as select 
				name,
				type as type_org,
				length
			from dictionary.columns 
			where libname='WORK' and memname="&padrao";
		quit;
		proc sql;
			create table str_TabelaBruta as select 
				name,
				type 
			from dictionary.columns 
			where libname="&lib" and memname="&nome";
		quit;
		data erro;
			length TIPO $ 10 DESCRICAO $ 100 TABELA1 $ 30 TABELA2 $ 30;
			if _N_=1 then do;
				if 0 then set work.str_TabelaBruta;
				declare hash P(dataset:"work.str_TabelaBruta");
				P.definekey('name');      
				P.definedata('type');
				P.definedone();
			end;
			set str_Tabela;
			if P.find()~=0 then do;
				TIPO = 'ERRO';
				DESCRICAO = "Coluna: '" || trim(name) || "' não encontrada.";
				TABELA1 = "&nome.";
				TABELA2 = 'N/A';
				output;
			end;
			else if type ~= type_org then do;
				TIPO = 'ERRO';
				DESCRICAO = "Coluna: '" || trim(name) || "' tipo errado. Importar novamente.";
				TABELA1 = "&nome.";
				TABELA2 = 'N/A';
				output;
			end;
			keep TIPO DESCRICAO TABELA1 TABELA2;
		run;
		%local cnt i;
		proc sql noprint;
			select count(*) into :cnt from erro;
		quit;
		%if &cnt ~= 0 %then %do;
			%LOG_InsereTabela(erro)
			%let &rc = 1;
		%end;
	%end;
%mend log_verificaTabela;
%macro log_tabelas;
	/* CALENDARIO*/
	proc sql noprint;
		create table CALENDARIO_SETOR ( 
			COD_SETOR NUM FORMAT=BEST12.,
			SETOR CHAR(24),
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			ESTRATEGIA CHAR(1),
			ABERTURA NUM FORMAT=DATE9.,
			FECHAMENTO NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,CALENDARIO_SETOR,erro,CALENDARIO_SETOR)	
	/* CALENDARIZACAO*/
	proc sql noprint;
		create table CALENDARIZACAO_00 ( 
			ROTA NUM FORMAT=BEST12.,
			DOM CHAR(1),
			SEG CHAR(1),
			TER CHAR(1),
			QUA CHAR(1),
			QUI CHAR(1),
			SEX CHAR(1),
			SAB CHAR(1),
			HORA_CORTE NUM FORMAT=HHMM5.,
			DIA_CORTE NUM FORMAT=BEST12.,
			DIA_COLETA NUM FORMAT=BEST12.,
			HORA_COLETA NUM FORMAT=HHMM5.,
			STATUS CHAR(5),
			GRUPO CHAR(5),
			DATA_INICIO NUM FORMAT=DATE9.,
			DATA_FINAL NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(WORK,CALENDARIZACAO,erro,CALENDARIZACAO_00)
	/* CAPTACAO_DIA*/
	proc sql noprint;
		create table CAPTACAO_DIA_SETOR ( 
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			COD_SETOR NUM FORMAT=BEST12.,
			SETOR CHAR(24),
			DIA_SEMANA NUM FORMAT=BEST12.,
			'1'n NUM FORMAT=BEST12.,
			'2'n NUM FORMAT=BEST12.,
			'3'n NUM FORMAT=BEST12.,
			'4'n NUM FORMAT=BEST12.,
			'5'n NUM FORMAT=BEST12.,
			'6'n NUM FORMAT=BEST12.,
			'7'n NUM FORMAT=BEST12.,
			'8'n NUM FORMAT=BEST12.,
			'9'n NUM FORMAT=BEST12.,
			'10'n NUM FORMAT=BEST12.,
			'11'n NUM FORMAT=BEST12.,
			'12'n NUM FORMAT=BEST12.,
			'13'n NUM FORMAT=BEST12.,
			'14'n NUM FORMAT=BEST12.,
			'15'n NUM FORMAT=BEST12.,
			'16'n NUM FORMAT=BEST12.,
			'17'n NUM FORMAT=BEST12.,
			'18'n NUM FORMAT=BEST12.,
			'19'n NUM FORMAT=BEST12.,
			'20'n NUM FORMAT=BEST12.,
			'21'n NUM FORMAT=BEST12.,
			'22'n NUM FORMAT=BEST12.,
			'23'n NUM FORMAT=BEST12.,
			'24'n NUM FORMAT=BEST12.,
			'25'n NUM FORMAT=BEST12.,
			'26'n NUM FORMAT=BEST12.,
			'27'n NUM FORMAT=BEST12.,
			'28'n NUM FORMAT=BEST12., 
			'29'n NUM FORMAT=BEST12.,
			'30'n NUM FORMAT=BEST12.,
			'31'n NUM FORMAT=BEST12.,
			'32'n NUM FORMAT=BEST12.,
			'33'n NUM FORMAT=BEST12.,
			'34'n NUM FORMAT=BEST12.,
			'35'n NUM FORMAT=BEST12.);
	quit;
	%log_verificaTabela(SIMULA,CAPTACAO_DIA_SETOR,erro,CAPTACAO_DIA_SETOR)
	/* RELACAO_DIA = CAPTACAO_DIA*/
	%log_verificaTabela(SIMULA,RELACAO_DIA_SETOR,erro,CAPTACAO_DIA_SETOR)
	/* RELACAO_ITEM_DIA = CAPTACAO_DIA*/
	%log_verificaTabela(SIMULA,RELACAO_ITEM_DIA_SETOR,erro,CAPTACAO_DIA_SETOR)
	/* CD*/
	proc sql noprint;
		create table CD_00 ( 
			COD_CD NUM FORMAT=BEST12.,
			CD CHAR(24),
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(WORK,CD,erro,CD_00)
	/* CD_SETOR*/
	proc sql;
		create table CD_SETOR_ZONEAMENTO_00
		  (
		   COD_CD num format=BEST12. informat=BEST12.,
		   CD char(24) format=$CHAR24. informat=$CHAR24.,
		   COD_SETOR num format=BEST12. informat=BEST12.,
		   SETOR char(26) format=$CHAR26. informat=$CHAR26.,
		   ZONEAMENTO char(9) format=$CHAR9. informat=$CHAR9.,
		   DATA_INI num format=DATE.,
		   DATA_FIN num format=DATE.
		  );
	quit;
	%log_verificaTabela(WORK,CD_SETOR_ZONEAMENTO,erro,CD_SETOR_ZONEAMENTO_00)
	/* DEMANDA_ITENS*/
	proc sql noprint;
		create table DEMANDA_ITENS_RE ( 
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			DEMANDA_ITEM NUM FORMAT=COMMA12.);
	quit;
	%log_verificaTabela(SIMULA,DEMANDA_ITENS_RE,erro,DEMANDA_ITENS_RE)	
	/* DEMANDA_PEDIDOS*/
	proc sql noprint;
		create table DEMANDA_PEDIDOS_RE ( 
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			DEMANDA_PEDIDO NUM FORMAT=COMMA12.);
	quit;	
	%log_verificaTabela(SIMULA,DEMANDA_PEDIDOS_RE,erro,DEMANDA_PEDIDOS_RE)
	/* ESTRUTURA_COMERCIAL*/
	proc sql noprint;
		create table ESTRUTURA_COMERCIAL_00 ( 
			UR CHAR(21),
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			COD_GV NUM FORMAT=BEST12.,
			GV CHAR(21),
			COD_SETOR NUM FORMAT=BEST12.,
			SETOR CHAR(21),
			UF CHAR(2),
			NOME_UF CHAR(19),
			CIDADE CHAR(32),
			ZONEAMENTO CHAR(9),
			QTD_CN NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(WORK,ESTRUTURA_COMERCIAL,erro,ESTRUTURA_COMERCIAL_00)		
	/* FERIADO*/
	proc sql noprint;
		create table FERIADO ( 
			COD_CD NUM FORMAT=BEST12.,
			CD CHAR(24),
			DATA NUM FORMAT=PTGDFDE9.,
			DESCRICAO CHAR(36),
			FOLGA_NOTURNO NUM FORMAT=BEST12.);
	quit;
	%log_verificaTabela(SIMULA,FERIADO,erro,FERIADO)	
	/* ROTA_ZONEAMENTO*/
	proc sql noprint;
		create table ROTA_ZONEAMENTO_00 ( 
			ROTA NUM FORMAT=BEST12.,
			ZONEAMENTO CHAR(9),
/*			nome_cidade CHAR(36),*/
			PRAZO_CONTRATADO NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(WORK,ROTA_ZONEAMENTO,erro,ROTA_ZONEAMENTO_00)	
%mend log_tabelas;
%macro log_estruturas;
	/* ESTRUTURA_COMERCIAL*/
	proc sql noprint;
		create table ESTRUTURA_COMERCIAL ( 
			UR CHAR(21),
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			COD_GV NUM FORMAT=BEST12.,
			GV CHAR(21),
			COD_SETOR NUM FORMAT=BEST12.,
			SETOR CHAR(21),
			ZONEAMENTO CHAR(9),
			QTD_CN NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,ESTRUTURA_COMERCIAL,erro,ESTRUTURA_COMERCIAL)		

	/* ESTRUTURA_LOGISTICA*/
	PROC SQL;
	create table WORK.ESTRUTURA_LOGISTICA
	  (
		   COD_CD num format=BEST12. informat=BEST12.,
		   CD char(24) format=$CHAR24. informat=$CHAR24.,
		   TRANSPORTADORA char(30) format=$CHAR30. informat=$CHAR30.,
		   FILIAL char(50) format=$CHAR50. informat=$CHAR50.,
		   ROTA num format=BEST12. informat=BEST12.,
		   ZONEAMENTO char(9) format=$CHAR9. informat=$CHAR9.,
		   UF char(2) format=$CHAR2. informat=$CHAR2.,
		   NOME_UF char(19) format=$CHAR19. informat=$CHAR19.,
		   CIDADE char(40) format=$CHAR40. informat=$CHAR40.,
		   DURACAO_TOTAL num format=BEST12. informat=BEST12.,
		   DURACAO_CD_FILIAL num format=F12. informat=BEST12.,
		   'Vel Média -TP ZN'n num format=BEST12. informat=BEST12.,
		   'TEMPO MOVIM CARGA'n num format=BEST12. informat=BEST12.,
		   'TIPO TURNO MOV'n num format=BEST12. informat=BEST12.,
		   'TIPO TURNO EXP'n num format=BEST12. informat=BEST12.,
		   'TEMPO ENTREGA'n num format=BEST12. informat=BEST12.,
		   PRAZO_CONTRATADO num format=BEST12. informat=BEST12.,
		   DOM char(1) format=$CHAR1. informat=$CHAR1.,
		   SEG char(1) format=$CHAR1. informat=$CHAR1.,
		   TER char(1) format=$CHAR1. informat=$CHAR1.,
		   QUA char(1) format=$CHAR1. informat=$CHAR1.,
		   QUI char(1) format=$CHAR1. informat=$CHAR1.,
		   SEX char(1) format=$CHAR1. informat=$CHAR1.,
		   SAB char(1) format=$CHAR1. informat=$CHAR1.,
		   HORA_CORTE num format=HHMM5. informat=HHMM5.,
		   DIA_CORTE num format=BEST12. informat=BEST12.,
		   DIA_COLETA num format=BEST12. informat=BEST12.,
		   HORA_COLETA num format=HHMM5. informat=HHMM5.,
		   STATUS char(5) format=$CHAR5. informat=$CHAR5.,
		   GRUPO char(5) format=$CHAR5. informat=$CHAR5.,
		   COD_PA num format=BEST12. informat=BEST12.,
		   PA char(1) format=$CHAR1. informat=$CHAR1.,
		   CAPACIDADE_ESTRATEGIA num format=BEST12. informat=BEST12.,
		   CAPACIDADE_NORMAL num format=BEST12. informat=BEST12.,
		   SAI_SABADO num format=BEST12. informat=BEST12.,
		   SAI_DOMINGO num format=BEST12. informat=BEST12.,
		   ENTREGA_SABADO num format=BEST12. informat=BEST12.,
		   ENTREGA_DOMINGO num format=BEST12. informat=BEST12.,
		   DATA_INI num format=DATE9. informat=DATE9.,
		   DATA_FIN num format=DATE9. informat=DATE9.
	  );
    QUIT;
	%log_verificaTabela(SIMULA,ESTRUTURA_LOGISTICA,erro,ESTRUTURA_LOGISTICA)	
%mend log_estruturas;

%macro log_main;

%log_tabelas
%if &erro. = 0 %then %do;
	%log_estrutura_com

	%log_cd_setor
	%log_calendarizacao
	%log_calendario
	%log_captacao_dia
	%log_rota_transp_filial
	%log_relacao_dia
	%log_rota_zoneamento
	%log_demanda
%end;
PROC SQL;
	SELECT DISTINCT * FROM SIMULA.LOG ORDER BY TIPO DESC;
QUIT;
PROC SQL NOPRINT;
	SELECT COUNT(*) INTO :erro FROM SIMULA.LOG WHERE TIPO="ERRO";
QUIT;
%PUT ERRO=&ERRO.;
%mend log_main;
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
	for{st in setorSet, <st1,zn,di> in EstrComSet: st=st1 and di=dataIniEC} do;
		reSetor[st] = reEC[st,zn,di];
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
	set setorCDSet{cd in CDSet, d in Dias} = setof{<cd1,st,zn,di> in CDSetorZN: cd = cd1 and 
		d >= di and d <= dataFinCS[cd,st,zn,di]}st;


/******************* Cria conjunto de setor/zoneamento para um CD/Dia **************/
	set<num,num,num,num,num,str> sepSet init {};
	for{d in Dias, <cd,st,zn,di> in CDSetorZN: cd in CDSet and d>=di and d<=dataFinCS[cd,st,zn,di]}
		for{<cc,a> in cicloAnoSetor[(st),(d)]}
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
/*		put percCaptaDiaCC[ciclo,ano,st,ds,dt,dcc]=;*/
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
			for{<cd,st,(zn),di3> in CDSetorZN: cd in CDSet and di3<=CALfin1 and dataFinCS[cd,st,zn,di3]>=CALini1} do;
				CALini2 = max(di3,CALini1);
				CALfin2 = min(dataFinCS[cd,st,zn,di3],CALfin1);
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

/*	Dias = firstDay..(firstDay+15);*/
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
DATA WORK.DEMANDA_ITENS;
	SET SIMULA.DEMANDA_ITENS_RE;
	RENAME DEMANDA_ITEM=ITEM1;
RUN;
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
	inner join WORK.ESTRUTURA_COMERCIAL AS t2 
		on t1.GV=t2.COD_GV AND t1.COD_SETOR = t2.COD_SETOR AND t1.ZONEAMENTO=t2.ZONEAMENTO
	inner join WORK.CD AS t3 on t1.COD_CD = t3.COD_CD
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
	LEFT join WORK.PA_ROTA AS t2 
		on t1.ROTA=t2.ROTA AND t1.DATA >= t2.DATA_INI AND t1.DATA <= t2.DATA_FIN
	LEFT join WORK.ROTA_TRANSPORTADORA_FILIAL AS t3 
		on t1.ROTA=t3.ROTA AND t1.DATA >= t3.DATA_INI AND t1.DATA <= t3.DATA_FIN
	;
QUIT;

%mend rel_demanda_detalhada;


%macro main;
%global erro;
%let erro = 0;
proc sql;
	create table simula.log (TIPO CHAR(10), DESCRICAO CHAR(200), TABELA1 CHAR(30), TABELA2 CHAR(30));
quit;
%log_estruturas
%init_nova_estrutura
%log_main
%if &erro = 0 %then %do;
	%simula
	%rel_demanda_detalhada
%end;
/* LIMPA O LIXO*/
proc datasets library=work kill nolist;
quit;
%mend main;
%main;


