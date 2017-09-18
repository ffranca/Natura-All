%macro log_estrutura_com;
/*Log 1: ESTRUTURA_COMERCIAL	AVISO	Setor/zoneamento sem cadastro de xx/xx/xx a xx/xx/xx */
/*Log 2: ESTRUTURA_COMERCIAL	AVISO	Setor/zoneameto com cadastro duplicado entre xx/xx/xx e xx/xx/xx*/
proc sort data=simula.estrutura_comercial;
	by cod_setor zoneamento data_ini;
run;

data log1e2;
	set simula.estrutura_comercial;
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
      FROM SIMULA.CD_SETOR t1 LEFT JOIN SIMULA.CD t2 ON (t1.COD_CD = t2.COD_CD)
      WHERE t2.COD_CD IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			trim(setor) || ' com CD inexistente(' || compress(put(cod_cd,8.)) || ') entre ' || 
				put(data_ini, date9.) || ' e ' || put(data_fin, date9.) as DESCRICAO,
			'CD_SETOR' AS TABELA1,
			'CD' AS TABELA2
		from WORK.LOG20_001;
quit;

/*Log 21: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor inexistente*/
PROC SQL;
   CREATE TABLE WORK.LOG21_001 AS 
   SELECT t1.COD_SETOR, 
          t1.SETOR, 
          t1.COD_CD, 
          t1.CD, 
          t1.DATA_INI, 
          t1.DATA_FIN
      FROM SIMULA.CD_SETOR t1 LEFT JOIN SIMULA.ESTRUTURA_COMERCIAL t2 ON (t1.COD_SETOR = t2.COD_SETOR)
      WHERE t2.COD_SETOR IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'Código do setor ' || trim(setor) || ' inexistente(' || compress(put(cod_setor,8.)) || 
				') entre ' || put(data_ini, date9.) || ' e ' || put(data_fin, date9.) as DESCRICAO,
			'CD_SETOR' AS TABELA1,
			'ESTRUTURA_COMERCIAL' AS TABELA2
		from WORK.LOG21_001;
quit;

/*Log 22a: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor sem separação entre xx/xx/xx e xx/xx/xx*/
/*Log 22b: CD_SETOR	ESTRUTURA_COMERCIAL	ERRO	Setor separado em mais de um CD entre xx/xx/xx e xx/xx/xx*/
proc sort data=simula.cd_setor;
	by cod_setor data_ini;
run;

data log22ab_001;
	set simula.cd_setor;
	by cod_setor;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.cod_setor then
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
			'Setor ' || compress(put(cod_setor,8.)) || ' - ' || trim(setor) || ' sem separação entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(data_ini-1, date9.) as DESCRICAO,
			'CD_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from log22ab_002;
quit;
proc sql;
	create table log22ab_003 as select * from log22ab_001
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'Setor ' || compress(put(cod_setor,8.)) || ' - ' || trim(setor) || ' com cadastro duplicado entre ' || 
				put(data_ini, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'CD_SETOR' AS TABELA1,
			'N/A' AS TABELA2
		from log22ab_003;
quit;
%mend log_cd_setor;
%macro log_calendario;
/*Log 11A: ESTRUTURA_COMERCIAL	CALENDARIO	ERRO	 GV sem calendário*/
PROC SQL;
   CREATE TABLE WORK.LOG11A_001 AS 
   SELECT t1.COD_GV, 
          t1.GV
      FROM SIMULA.ESTRUTURA_COMERCIAL t1 LEFT JOIN SIMULA.CALENDARIO t2 ON (t1.COD_RE = t2.COD_RE) AND (t1.COD_GV =
           t2.COD_GV)
      WHERE t2.COD_GV IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || ') sem calendário' as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'CALENDARIO' AS TABELA2
		from WORK.LOG11A_001;
quit;
/*Log 11B: CALENDARIO	CALENDARIO	ERRO	 GV sem ciclo entre xx/xx/xx e xx/xx/xx*/
proc sort data=simula.calendario;
	by cod_gv abertura;
run;

data log11b_001;
	set simula.calendario;
	by cod_gv;
	format last_fin date9. ini_gap date9. ini_ovl date9.;
	retain last_fin;

	if first.cod_gv then
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
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || ') sem ciclo entre ' || 
				put(ini_gap+1, date9.) || ' e ' || put(abertura-1, date9.) as DESCRICAO,
			'CALENDARIO' AS TABELA1,
			'N/A' AS TABELA2
		from log11b_002;
quit;
/*Log 12: CALENDARIO	CALENDARIO	ERRO	GV com mais de um ciclo entre xx/xx/xx e xx/xx/xx*/
proc sql;
	create table log11b_003 as select * from log11b_001
	where ovl_error = 1;
		
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || ') com mais de um ciclo entre ' || 
				put(abertura, date9.) || ' e ' || put(ini_ovl, date9.) as DESCRICAO,
			'CALENDARIO' AS TABELA1,
			'N/A' AS TABELA2
		from log11b_003;
quit;
%mend log_calendario;

%macro log_calendarizacao;
/*Log 7: CALENDARIZAÇÃO	PA_ROTA	AVISO	Rota inexistente*/
PROC SQL;
   CREATE TABLE WORK.LOG7_001 AS 
   SELECT t1.*
      FROM SIMULA.CALENDARIZACAO t1 LEFT JOIN SIMULA.PA_ROTA t2 ON (t1.ROTA = t2.ROTA)
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
      FROM SIMULA.CALENDARIZACAO t1
      WHERE t1.DOM IS MISSING AND t1.SEG IS MISSING AND t1.TER IS MISSING AND t1.QUA IS MISSING AND t1.QUI IS MISSING AND
            t1.SEX IS MISSING AND t1.SAB IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'Rota ' || compress(put(ROTA,8.)) || ' sem dia de entrega expressa' as DESCRICAO,
			'CALENDARIZACAO' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG8_001;
quit;
/*Log 9: CALENDARIZAÇÃO	CALENDARIZAÇÃO	AVISO	Dia de corte inválido*/
PROC SQL;
   CREATE TABLE WORK.LOG9_001 AS 
   SELECT t1.ROTA,t1.DIA_CORTE
      FROM SIMULA.CALENDARIZACAO t1
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
      FROM SIMULA.CALENDARIZACAO t1
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
/*Log 13: CAPTACAO_DIA	ESTRUTURA_COMERCIAL	AVISO	GV sem captação dia*/
PROC SQL;
   CREATE TABLE WORK.LOG13_001 AS 
   SELECT DISTINCT t1.COD_GV, 
          t1.GV
      FROM SIMULA.ESTRUTURA_COMERCIAL t1 LEFT JOIN SIMULA.CAPTACAO_DIA t2 ON (t1.COD_RE = t2.COD_RE) AND (t1.COD_GV =
           t2.COD_GV)
      WHERE t2.COD_GV IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || ') sem captação dia' as DESCRICAO,
			'ESTRUTURA_COMERCIAL' AS TABELA1,
			'CAPTACAO_DIA' AS TABELA2
		from WORK.LOG13_001;
quit;
/*Log 14a: CAPTACAO_DIA	CALENDARIO	AVISO	Captação com número de dias de ciclo diferente do CALENDARIO*/
PROC SQL;
   CREATE TABLE WORK.LOG14A_001 AS 
   SELECT DISTINCT t1.CICLO, 
          t1.ANO, 
          t1.COD_RE, 
          t1.RE, 
          t1.COD_GV, 
          t1.GV, 
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
          t2.ABERTURA, 
          t2.FECHAMENTO
      FROM SIMULA.CAPTACAO_DIA t1, SIMULA.CALENDARIO t2
      WHERE (t1.COD_RE = t2.COD_RE AND t1.COD_GV = t2.COD_GV AND t1.CICLO = t2.CICLO AND t1.ANO = t2.ANO AND
            (weekday(t2.ABERTURA)=t1.DIA_SEMANA));
QUIT;
data LOG14A_002;
	set LOG14A_001;
	array dia(28) d1-d28;
	count = 0;
	do i=1 to 28;
		if dia[i] ~= 0 then count = count + 1;
	end;
	if count ~= (fechamento - abertura + 1) then output;
run;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - captação com número de dias de ciclo diferente do CALENDARIO' as DESCRICAO,
			'CAPTACAO_DIA' AS TABELA1,
			'CALENDARIO' AS TABELA2
		from WORK.LOG14A_002;
quit;
/*Log 14b: CAPTACAO_DIA	CALENDARIO	AVISO	GV sem captação para o ano ciclo*/
PROC SQL;
   CREATE TABLE WORK.LOG14B_001 AS 
   SELECT DISTINCT t2.COD_RE, 
          t2.RE, 
          t2.COD_GV, 
          t2.GV, 
          t2.CICLO, 
          t2.ANO, 
          t2.ESTRATEGIA, 
          t2.ABERTURA, 
          t2.FECHAMENTO
      FROM SIMULA.CAPTACAO_DIA t1 RIGHT JOIN SIMULA.CALENDARIO t2 ON (t1.COD_RE = t2.COD_RE) AND (t1.COD_GV = t2.COD_GV)
           AND (t1.CICLO = t2.CICLO) AND (t1.ANO = t2.ANO AND (weekday(t2.ABERTURA)=t1.DIA_SEMANA))
      WHERE t1.COD_GV IS MISSING;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'AVISO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - sem captação dia' as DESCRICAO,
			'CAPTACAO_DIA' AS TABELA1,
			'CALENDARIO' AS TABELA2
		from WORK.LOG14B_001;
quit;
/*Log 15: CAPTACAO_DIA	CAPTACAO_DIA	ERRO	Dia da semana inválido*/
PROC SQL;
   CREATE TABLE WORK.LOG15_001 AS 
   SELECT t1.COD_GV, 
          t1.GV, 
          t1.DIA_SEMANA
      FROM SIMULA.CAPTACAO_DIA t1
      WHERE t1.DIA_SEMANA NOT BETWEEN 1 AND 7;
QUIT;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || 
				') - dia da semana inválido(' || compress(put(dia_semana,8.)) || ')' as DESCRICAO,
			'CAPTACAO_DIA' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG15_001;
quit;
/*Log 16: CAPTACAO_DIA	CAPTACAO_DIA	ERRO	Captação total dierente de 100%*/
data LOG16_001;
	set LOG14A_001;
	array dia(28) d1-d28;
	total = 0;
	do i=1 to 28;
		total = total + dia[i];
	end;
	if abs(total - 1) >= 0.01 then output;
run;
proc sql;
	insert into simula.log 
		select 
			'ERRO' as TIPO, 
			'GV ' || trim(gv) || '(' || compress(put(cod_gv,8.)) || 
				') ciclo ' || compress(put(ciclo,8.)) || '/' || compress(put(ano,8.)) ||
				' - captação total diferente de 100%(' || compress(put(total,percent20.2)) || ')' as DESCRICAO,
			'CAPTACAO_DIA' AS TABELA1,
			'N/A' AS TABELA2
		from WORK.LOG16_001;
quit;
%mend log_captacao_dia;
%macro log_rota_transp_filial;
/*AVISO	Rota sem transportadora de xx/xx/xx a xx/xx/xx*/
proc sort data=simula.rota_transportadora_filial;
	by rota data_ini;
run;

data log1e2;
	set simula.rota_transportadora_filial;
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
proc sort data=simula.&tabela.;
	by &ord1. data_ini;
run;

data log1e2;
	set simula.&tabela.;
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
      FROM SIMULA.&tabela. t1 LEFT JOIN SIMULA.CD t2 ON (t1.COD_CD = t2.COD_CD)
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
      FROM SIMULA.CD t2 LEFT JOIN SIMULA.&tabela. t1 ON (t2.COD_CD = t1.COD_CD)
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
      FROM SIMULA.&tabela1. t1 
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
%macro verifica_chave1(tabela1,tabela2,chave1,tipo);
/*AVISO	chave inexistente*/
PROC SQL;
   CREATE TABLE WORK.log_chv_001 AS 
   SELECT DISTINCT t1.&chave1.
      FROM SIMULA.&tabela1. t1 LEFT JOIN SIMULA.&tabela2. t2 ON t1.&chave1.=t2.&chave1.
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
      FROM SIMULA.&tabela2. t2 LEFT JOIN SIMULA.&tabela1. t1 ON (t1.&chave1.=t2.&chave1.)
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
      FROM SIMULA.&tabela1. t1
      GROUP BY t1.&chave1.;
QUIT;

PROC SQL;
   CREATE TABLE WORK.LOG_TMP2_002 AS 
   SELECT distinct t2.&chave1., 
          t2.DATA_INI, 
          t2.DATA_FIN, 
          t1.MIN_of_DATA_INI, 
          t1.MAX_of_DATA_FIN
      FROM WORK.LOG_TMP2_001 t1, SIMULA.&tabela2. t2
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
%verifica_chave2(RELACAO_DIA, ESTRUTURA_COMERCIAL, COD_RE, COD_GV,AVISO);

%mend log_relacao_dia;
%macro log_rota_zoneamento;
%verifica_chave1(ROTA_ZONEAMENTO,PA_ROTA,rota);
%verifica_chave1(ESTRUTURA_COMERCIAL,ROTA_ZONEAMENTO,zoneamento,CHAR);
%verifica_chave0(ROTA_ZONEAMENTO,zoneamento,CHAR,rota);
%verifica_chave0(ROTA_ZONEAMENTO,rota,NUM,zoneamento);
%verifica_tempo(ROTA_ZONEAMENTO,zoneamento,CHAR);
%verifica_tempo1(ESTRUTURA_COMERCIAL,ROTA_ZONEAMENTO,zoneamento,CHAR);
%mend log_rota_zoneamento;
%macro log_demanda;
%verifica_chave2(DEMANDA_PEDIDOS,CALENDARIO,CICLO,ANO,ERRO);
%verifica_chave2(DEMANDA_ITENS,CALENDARIO,CICLO,ANO,ERRO);

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
		create table CALENDARIO ( 
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			COD_GV NUM FORMAT=BEST12.,
			GV CHAR(23),
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			ESTRATEGIA CHAR(1),
			ABERTURA NUM FORMAT=DATE9.,
			FECHAMENTO NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,CALENDARIO,erro,CALENDARIO)	
	/* CALENDARIZACAO*/
	proc sql noprint;
		create table CALENDARIZACAO ( 
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
	%log_verificaTabela(SIMULA,CALENDARIZACAO,erro,CALENDARIZACAO)
	/* CAPTACAO_DIA*/
	proc sql noprint;
		create table CAPTACAO_DIA ( 
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			COD_GV NUM FORMAT=BEST12.,
			GV CHAR(23),
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
			'28'n NUM FORMAT=BEST12.);
	quit;
	%log_verificaTabela(SIMULA,CAPTACAO_DIA,erro,CAPTACAO_DIA)
	/* RELACAO_DIA = CAPTACAO_DIA*/
	%log_verificaTabela(SIMULA,RELACAO_DIA,erro,CAPTACAO_DIA)
	/* RELACAO_ITEM_DIA = CAPTACAO_DIA*/
	%log_verificaTabela(SIMULA,RELACAO_ITEM_DIA,erro,CAPTACAO_DIA)
	/* CD*/
	proc sql noprint;
		create table CD ( 
			COD_CD NUM FORMAT=BEST12.,
			CD CHAR(24),
			CIDADE CHAR(17),
			RE CHAR(8),
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,CD,erro,CD)
	/* CD_SETOR*/
	proc sql noprint;
		create table CD_SETOR ( 
			COD_RE NUM FORMAT=BEST12.,
			RE CHAR(12),
			COD_GV NUM FORMAT=BEST12.,
			GV CHAR(21),
			COD_SETOR NUM FORMAT=BEST12.,
			SETOR CHAR(21),
			COD_CD NUM FORMAT=BEST12.,
			CD CHAR(24),
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,CD_SETOR,erro,CD_SETOR)
	/* DEMANDA_ITENS*/
	proc sql noprint;
		create table DEMANDA_ITENS ( 
			CICLO NUM FORMAT=BEST12.,
			ANO NUM FORMAT=BEST12.,
			SUL NUM FORMAT=COMMA12.,
			'SP CAPITAL'n NUM FORMAT=COMMA12.,
			'RJ-ES'n NUM FORMAT=COMMA12.,
			MG NUM FORMAT=COMMA12.,
			'SP INTERIOR'n NUM FORMAT=COMMA12.,
			'CENTRO OESTE'n NUM FORMAT=COMMA12.,
			NORDESTE NUM FORMAT=COMMA12.,
			NORTE NUM FORMAT=COMMA12.);
	quit;
	%log_verificaTabela(SIMULA,DEMANDA_ITENS,erro,DEMANDA_ITENS)	
	/* DEMANDA_PEDIDOS = DEMANDA_ITENS*/
	%log_verificaTabela(SIMULA,DEMANDA_PEDIDOS,erro,DEMANDA_ITENS)
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
			UF CHAR(2),
			NOME_UF CHAR(19),
			CIDADE CHAR(32),
			ZONEAMENTO CHAR(9),
			QTD_CN NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,ESTRUTURA_COMERCIAL,erro,ESTRUTURA_COMERCIAL)	
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
	/* PA*/
	proc sql noprint;
		create table PA ( 
			COD_PA NUM FORMAT=BEST12.,
			PA CHAR(24),
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,PA,erro,PA)
	/* PA_ROTA*/
	proc sql noprint;
		create table PA_ROTA ( 
			ROTA NUM FORMAT=BEST12.,
			COD_PA NUM FORMAT=BEST12.,
			PA CHAR(20),
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,PA_ROTA,erro,PA_ROTA)	
	/* ROTA_TRANSPORTADORA_FILIAL*/
	proc sql noprint;
		create table ROTA_TRANSPORTADORA_FILIAL ( 
			TRANSPORTADORA CHAR(9),
			FILIAL CHAR(30),
			ROTA NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,ROTA_TRANSPORTADORA_FILIAL,erro,ROTA_TRANSPORTADORA_FILIAL)		
	/* ROTA_ZONEAMENTO*/
	proc sql noprint;
		create table ROTA_ZONEAMENTO ( 
			ROTA NUM FORMAT=BEST12.,
			ZONEAMENTO CHAR(9),
			nome_cidade CHAR(36),
			PRAZO_CONTRATADO NUM FORMAT=BEST12.,
			DATA_INI NUM FORMAT=DATE9.,
			DATA_FIN NUM FORMAT=DATE9.);
	quit;
	%log_verificaTabela(SIMULA,ROTA_ZONEAMENTO,erro,ROTA_ZONEAMENTO)	
%mend log_tabelas;
%macro log_main;
%global erro;
%let erro = 0;
proc sql;
	create table simula.log (TIPO CHAR(10), DESCRICAO CHAR(100), TABELA1 CHAR(30), TABELA2 CHAR(30));
quit;
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
%log_main;