%let BACKLOG_INICIAL=0;
%let DEMANDA_DETALHADA=1;
%let NS_DETALHADO=0;
%let RODAR_SIMULACAO=1;
%let SAIDA_PADRAO=1;
OPTIONS VALIDVARNAME=ANY;
%macro ExtendValidMemName;

%if %sysevalf(&sysver>=9.3) %then options validmemname=extend;

%mend ExtendValidMemName;

%ExtendValidMemName;
libname SIMULA "E:\logical\projetos\Natura\Manutenção 2023\dados_novos";
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

%macro leDados;
/******************************* CARREGA TABELAS SIMULA ***************************/
/* Carrega DEMANDA_PEDIDOS*/
	set<num,num> DemandaSet;
	set<num,num,num> DemSet;
	num demPedido{DemSet};
	read data simula.DEMANDA_PEDIDOS_RE into DemSet=[CICLO ANO COD_RE] demPedido=DEMANDA_PEDIDO;
/*	print demPedido;*/
	DemandaSet = setof{<ciclo,ano,re> in DemSet} <ciclo,ano>;
/*	put DemandaSet=;*/

/* Carrega DEMANDA_R$*/
/*	num demReais{DemandaSet,nRegDemanda};*/
/*	read data simula.DEMANDA_RS into DemandaSet=[CICLO ANO]*/
/*		{reg in nRegDemanda} <demReais[ciclo,ano,reg] = col(RegDemanda[reg])>;*/
/*	print demReais;*/

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
/*	read data simula.CD(where=(DATA_INI>"01jan2010"d)) into CDSet=[COD_CD] dataIniCD=DATA_INI CDNome=CD dataFinCD=DATA_FIN;*/
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

/* Carrega CALENDARIO_SETOR*/
	set<num,num,num> Calendario;
	num abreCiclo{Calendario};
	num abreCicloOrg{Calendario};
	num fechaCiclo{Calendario};
	str estrategiaCal{Calendario};
	read data simula.CALENDARIO_SETOR into Calendario=[CICLO ANO COD_SETOR]
		abreCiclo=ABERTURA abreCicloOrg=ABERTURA fechaCiclo=FECHAMENTO estrategiaCal=ESTRATEGIA;
/*	print abreCiclo fechaCiclo;*/

/* Carrega CAPTACAO_HORA**********************/
	set<num,num,num,num> CaptaHora;
	set horaCapta = {0..23};
	num percCaptaHoraAcc{CaptaHora,horaCapta};
	num percCaptaHora{CaptaHora,horaCapta};
	read data simula.CAPTACAO_HORA into CaptaHora=[COD_RE COD_GV DIA_SEMANA DATA]
	{hora in horaCapta} 
		<percCaptaHoraAcc[COD_RE,COD_GV,DIA_SEMANA,DATA,hora] = 
			col(put(hora,z2.)||':00:00')>;
/*	print percCaptaHoraAcc;*/
	for{<re,gv,ds,dia> in CaptaHora, hr in horaCapta} do;
		if hr = 0 then
			percCaptaHora[re,gv,ds,dia,hr] = percCaptaHoraAcc[re,gv,ds,dia,hr];
		else
			percCaptaHora[re,gv,ds,dia,hr] = 
				percCaptaHoraAcc[re,gv,ds,dia,hr] - percCaptaHoraAcc[re,gv,ds,dia,hr-1];
	end;
/*	print percCaptaHora percCaptaHoraAcc;*/

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

/* Carrega ENTREGA_EXPRESSA*/
	set<num,num> EntregaExpr init {};
	str diaSemEE{1..7} = ['DOM' 'SEG' 'TER' 'QUA' 'QUI' 'SEX' 'SAB'];
	num diaEE{EntregaExpr,1..7} init 0;
	str tmpDiaEE{EntregaExpr,1..7};
	num horaCorteEE{EntregaExpr};
	num diaCorteEE{EntregaExpr};
	num horaColetaEE{EntregaExpr};
	num diaColetaEE{EntregaExpr};
	str statusEE{EntregaExpr};
	num grupoEE{EntregaExpr};
	num dataFinEE{EntregaExpr};
	read data simula.ENTREGA_EXPRESSA into EntregaExpr=[ROTA DATA_INICIO]
		horaCorteEE=HORA_CORTE diaCorteEE=DIA_CORTE diaColetaEE=DIA_COLETA horaColetaEE=HORA_COLETA statusEE=STATUS 
		dataFinEE=DATA_FINAL grupoEE=COD_GRUPO_EE
		{d in 1..7} <tmpDiaEE[ROTA,DATA_INICIO,d] = col(diaSemEE[d])>;
	for{<z,di> in EntregaExpr, d in 1..7}
		if tmpDiaEE[z,di,d] ~= '' then diaEE[z,di,d] = 1;
	EntregaExpr = EntregaExpr diff {<0,'01jan2000'd>};
/*	print diaEE;*/

/* Carrega CADASTRO_DE_LINHAS*/
	set<num,num> CDLinha;
	num dataIniCL{CDLinha};
	num dataFinCL{CDLinha};
	read data simula.CADASTRO_DE_LINHAS into CDLinha=[COD_CD LINHA] dataIniCL=DATA_INI dataFinCL=DATA_FIN;
/*	print dataIniCL dataFinCL;*/

/* Carrega TEMPO_LINHA_SEPARACAO*/
	set<num,num> CDLinhaTurno;
	num tempoSep{CDLinhaTurno};
	num tempoExp{CDLinhaTurno};
	num dataFinLT{CDLinhaTurno};
	read data simula.TEMPO_LINHA_SEPARACAO into CDLinhaTurno=[COD_CD DATA_INI] 
		tempoSep=TEMPO_SEPARACAO tempoExp=TEMPO_EXPEDICAO dataFinLT=DATA_FIN;
/*	print tempoSep tempoExp dataFinLT;*/

/* Carrega VOLUME_HORA*/
	set<num,num,num,num> VolumeHoraSet;
	num volume_hora{VolumeHoraSet};
	num dataFinVH{VolumeHoraSet};
	read data simula.volume_hora into VolumeHoraSet=[cod_cd linha turno data_ini] 
		volume_hora dataFinVH=DATA_FIN;
/*	print volume_hora dataFinVH;*/

/* Carrega TURNO*/
	set<num,num,num,num> CDTurno;
	num iniTurno{CDTurno};
	num fimTurno{CDTurno};
	num refeicao{CDTurno};
	num dataFinTN{CDTurno};
	num turno_DOW{CDTurno,1..7};
	read data simula.turno into CDTurno=[cod_cd linha turno data_ini] 
		iniTurno=INICIO fimTurno=FIM refeicao dataFinTN=DATA_FIN
		{i in 1..7} <turno_DOW[cod_cd, linha, turno, data_ini,i] = col(substr(DOW[i],1,3))>;
	set Turno = setof{<cd,linha,turno,di> in CDTurno}<turno>;
/*	print iniTurno fimTurno refeicao dataFinTN;*/

/* Carrega HORA_EXTRA*/
	set<num,num,num,num> HoraExtraSet init {};
	num horaIniHE{HoraExtraSet};
	num horaFinHE{HoraExtraSet};
	num eficienciaHE{HoraExtraSet};
	read data simula.HORA_EXTRA into HoraExtraSet=[DATA COD_CD LINHA TURNO]
		horaIniHE=HORA_INI horaFinHE=HORA_FIN eficienciaHE=EFICIENCIA;
	HoraExtraSet = HoraExtraSet diff {<'01jan2000'd,0,0,0>};
/*	print horaIniHE horaFinHE eficienciaHE;*/

/* Carrega FERIADO*/
	set<num,num> Feriado;
	num folgaNoturno{Feriado};
	read data simula.FERIADO(where=(tipo='F')) into Feriado=[COD_CD DATA] folgaNoturno=FOLGA_NOTURNO;
/*	print folgaNoturno tipoFeriado;*/

/* Carrega CD_DIA_SEMANA*/
	set<num,num,num> CDDiaSemana;
	str tipoCDDiaSem{CDDiaSemana};
	num dataFinCDDiaSem{CDDiaSemana};
	read data simula.CD_DIA_SEMANA into CDDiaSemana=[COD_CD DIA_SEMANA DATA_INI]
		tipoCDDiaSem=TIPO dataFinCDDiaSem=DATA_FIN;
/*	print tipoCDDiaSem dataFinCDDiaSem; */

/* Carrega BACKLOG*/
	set<num,str> BacklogSet;
	num pedidoBK{BacklogSet};
	num volumeBK{BacklogSet};
	read data simula.BACKLOG into BacklogSet=[COD_SETOR ZONEAMENTO]
		pedidoBK=PEDIDO volumeBK=VOLUME;
/*	print pedidoBK; */

%mend leDados;
%let teste = 0;
%let ns_max = 240;
%macro timing;
	current = time();
	put current= time.;
%mend timing;

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

/* Constroi uma capCDHora por cd/dia/hora*/
	set<num,num> CDDia = setof{cd in CDSet, d in firstDay..(lastDay+30)}<cd,d>;
	set<num,num,num> CDDiaHora = CDDia cross horaCapta;
/*	put CDDiaHora = ;*/
	num capCDHora{CDDiaHora} init 0;
	num capTotalCDHora{CDDiaHora} init 0;
	num cini, cfim, cref, cIsTURno, cVolume;
	for{<cd,d> in CDDia} do;
		for{<ln,tr,di> in slice(<cd,*,*,*>,CDTURno), div in slice(<cd,ln,tr,*>, VolumeHoraSet): 
		di <= d and d <= dataFinTN[cd,ln,tr,di] and d>=div and d<=dataFinVH[cd,ln,tr,div]} do;
			cini = iniTURno[cd,ln,tr,di]/3600;
			cfim = fimTURno[cd,ln,tr,di]/3600;
			cref = refeicao[cd,ln,tr,di]/3600;
			cIsTURno = tURno_DOW[cd,ln,tr,di,weekday(d)];
			cVolume = volume_hora[cd,ln,tr,div];
			/* Caso do 3o tURno que termina no dia seguinte*/
			for{hr in horaCapta: hr ~= cref} do;
				if cini > cfim then do;
					if cIsTURno = 1 and hr >= cini then do;
						/* Verifica se é feriado*/
						if not(<cd,d+1> in Feriado and folgaNotURno[cd,d+1] = -1) and 
						   not(<cd,d> in Feriado and folgaNotURno[cd,d] = 0) then
							capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume;
					end;
					if tURno_DOW[cd,ln,tr,di,weekday(d-1)] = 1 and hr < cfim then do;
						/* Verifica se é feriado*/
						if not(<cd,d> in Feriado and folgaNotURno[cd,d] = -1) and 
						   not(<cd,d-1> in Feriado and folgaNotURno[cd,d-1] = 0) then
							capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume;
					end;
					/* Verifica hora extra*/
					if <d,cd,ln,tr> in HoraExtraSet then do;
						if horaIniHE[d,cd,ln,tr] = .  or hr >= hoUR(horaIniHE[d,cd,ln,tr]) then do;
							if hr >= cini then do;
								capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume*eficienciaHE[d,cd,ln,tr];
							end;
						end;
					end;
					if <d-1,cd,ln,tr> in HoraExtraSet then do;
						if horaFinHE[d-1,cd,ln,tr] = . or hr < hoUR(horaFinHE[d-1,cd,ln,tr]) then do;
							if hr < cfim then do;
								capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume*eficienciaHE[d-1,cd,ln,tr];
							end;
						end;
					end;
				end;
				/* Outros tURnos*/
				else do;
					if cIsTURno = 1 and hr>=cini and hr<cfim and <cd,d> not in Feriado then
						capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume;
					/* Verifica hora extra*/
					if <d,cd,ln,tr> in HoraExtraSet then do;
						if horaIniHE[d,cd,ln,tr] = . or (hr>=hoUR(horaIniHE[d,cd,ln,tr]) 
							and hr<hoUR(horaFinHE[d,cd,ln,tr]))then
							if hr>=cini and hr<cfim then
								capCDHora[cd,d,hr] = capCDHora[cd,d,hr] + cVolume*eficienciaHE[d,cd,ln,tr];
					end;
				end;
				capTotalCDHora[cd,d,hr] = capCDHora[cd,d,hr];
			end;
		end;
	end;
/*	for{<cd,d,hr> in CDDiaHora: cd = 2800 and d in '5sep2011'd..('12sep2011'd)} do;*/
/*		put 'Dia =' (put(d,date.)) '-' (weekday(d)) '' capCDHora[cd,d,hr]=;*/
/*	end;*/
/******************** Tempo de separação **************************************/
/* Fazer média ponderada(volume_hora) por CD*/
	num tempoSepCD{CDSet} init 5;
	for{<cd,di> in CDLinhaTurno: cd in CDSet}
		tempoSepCD[cd] = tempoSep[cd,di]+tempoExp[cd,di];
/*	print tempoSepCD;*/

/******************* Cria conjunto de setor/zoneamento para um CD/Dia **************/
/*	set<num,num,num,str> sepSet init {};*/
/*	for{d in Dias, <cd,st,di> in CDSetor: cd in CDSet and d>=di and d<=dataFinCS[cd,st,di]}*/
/*		for{zn in zoneSetorSet[st]}*/
/*			sepSet = sepSet union {<cd,d,st,zn>};*/

/* Nível de Serviço*/
	/*cd,dia,st,zn,ns,tipo*/
	set<num,num,num,num,str> NSSet init {};
	str CNS_calend = 'CAL';	str CNS_EE = 'EE'; str CNS_diario = 'DIARIO';
	str CVIOL_calend = 'CAL';	str CVIOL_EE = 'EE';
	num volNS{NSSet};
	set<num,num,num,num,str> NSUSet init {};
	num volNSU{NSUSet};
/* Atraso na Coleta*/
	set<num,num,num,str> AtrasaColetaSet init {};
	num volAtrasa{AtrasaColetaSet} init 0;
/* Variáveis privadas*/
	num lresto, lsep, ldia, lhora, lDiaColeta, lHoraColeta, lns, lnsu;
	num ldiafin, lhorafin, lrestoini;
	num lciclo, lano, lgv, ldiacc, ldowcc, lre, lped, lvol, litem, lrota, lexit;
	str lestr;
/* CD sem capacidade*/
	num semCapacidade{CDSet} init 0;

/* Faz capacidade agregada por dia, semana/ano e mes/ano (melhoria de performance) 40s*/
	num capCDDia{CDDia} init 0;
	for{<cd,dia> in CDDia} do;
		for{hr in 0..23}
			capCDDia[cd,dia] = capCDDia[cd,dia] + capCDHora[cd,dia,hr];
	end;
/* Por semana*/
	set<num,num,num> CDSemana = setof{<cd,dia> in CDDia}<cd,year(dia),week(dia)>;
	num capCDSemana{CDSemana} init 0;
	num firstDayCDSemana{CDSemana} init 1E10;
	for{<cd,dia> in CDDia} do;
		if firstDayCDSemana[cd,year(dia),week(dia)] > dia then firstDayCDSemana[cd,year(dia),week(dia)] = dia;
		capCDSemana[cd,year(dia),week(dia)] = capCDSemana[cd,year(dia),week(dia)] + capCDDia[cd,dia];
	end;
	num lcapdia, lsem;

/*************************** Estruturas por Rota ****************************/

	set Rotas = setof{<rt,zn,di> in RotaZone} rt;
	set<num,num,num> CDDiaRota;
	num volDiarioRota{CDDiaRota};
	num volCALRota{CDDiaRota};
	num volEERota{CDDiaRota} init 0;
	read data cd_dia_rota into CDDiaRota=[COD_CD DATA ROTA]
		volDiarioRota = VOLUME_DIARIO
		volCALRota = VOLUME_CALEND;

/*************************** Estruturas de captação ****************************/
	/* Loop CD\dia*/
	num backlog_calend{CDDia} init 0;
/*	num pedCapta_calend{sepSet} init 0;*/
/*	num volCapta_calend{sepSet} init 0;*/
/*	num itemCapta_calend{sepSet} init 0;*/
	set<num,num,num,num> progCalend init {};
	num volProg_calend{progCalend} init 0;

	num backlog_EE{CDDia} init 0;
/*	num pedCapta_EE{sepSet} init 0;*/
/*	num volCapta_EE{sepSet} init 0;*/


	num backlog_diario{CDDia} init 0;

/*	num pedCapta_diario{sepSet} init 0;*/
/*	num volCapta_diario{sepSet} init 0;*/
/*	num itemCapta_diario{sepSet} init 0;*/

/*	num rotaZnDia{sepSet} init 0;*/
/*	num gvZnDia{sepSet} init 0;*/
/* separação*/
	set<num,num,num,num> sepCalend init {};
	num separa_calend{sepCalend} init 0;
	set<num,num,num,num> sepEE init {};
	num separa_EE{sepEE} init 0;
	set<num,num,num,num> sepDiario init {};
	num separa_diario{sepDiario} init 0;
/*************************** Teste detalhado ==> desativar no programa final*/
%if &teste. = 1 %then %do;
	set teste_rt = Rotas;
	num dia_ini_teste = '29aug2019'd;
	num dia_fin_teste = '29aug2019'd;
	set<num,num,num,num,num> TesteHora init {};
	num tVolCapta{TesteHora} init 0;
	num tVolumeSepara{TesteHora} init 0;
	num tDiaSepFin{TesteHora} init 0;
	num tHoraSepFin{TesteHora} init 0;
	num tDiaColeta{TesteHora} init 0;
	num tHoraColeta{TesteHora} init 0;
	num tBacklogIni{TesteHora} init 0;
	num tBacklogFin{TesteHora} init 0;
	num tNS{TesteHora} init 0;
%end;
%mend Logistica_init;
%macro Logistica_procura_cap;
/* Identifica o primeiro dia partindo de ldia onde haja capacidade de separacao ==> devolve em lcapdia*/
/* Melhoria da performance ==> já coloca o volume no backlog*/
	lcapdia = ldia;
	if capCDDia[cd,lcapdia] < 0.1 then do;
		lano = year(ldia);
		lsem = week(ldia);
		lexit = 0;
		do while (lexit = 0);
			if capCDSemana[cd,lano,lsem] > 0.1 then do;
				if lcapdia < firstDayCDSemana[cd,lano,lsem] then lcapdia = firstDayCDSemana[cd,lano,lsem];
				lexit = 1;
			end;
			else do;
				if lano = year(lastDay+29) and lsem = week(lastDay+29) then do;
					lcapdia = firstDayCDSemana[cd,lano,lsem];
					lexit = 1;
				end;
				else if lsem >= 52 then do;
					lano = lano + 1;
					if <cd,lano,0> in CDSemana then lsem = 0;
					else lsem = 1;
				end;
				else lsem = lsem + 1;
			end;
		end;
		lexit = 0;
		do while (lcapdia <= lastDay+29 and lexit = 0);
			if capCDDia[cd,lcapdia] > 0.1 then lexit = 1;
			else lcapdia = lcapdia + 1;
		end;
	end;
/*	lcapdia=ldia;*/
%mend Logistica_procura_cap;
%macro Logistica_teste;
/*	if <zn,dia,hr,ldia,lhora> not in TesteHora then do;*/
		TesteHora = TesteHora union {<rt,dia,hr,ldia,lhora>};
/*	end;*/
/*	put TesteHora=;*/
/*	put st= zn= cd= dia= (put(dia,date.))= hr= (put(ldia,date.))= lhora=;*/
	tVolCapta[rt,dia,hr,ldia,lhora] = lrestoini;
	tVolumeSepara[rt,dia,hr,ldia,lhora] = lsep;
	tDiaColeta[rt,dia,hr,ldia,lhora] = lDiaColeta;
	tHoraColeta[rt,dia,hr,ldia,lhora] = lHoraColeta;
	tBacklogIni[rt,dia,hr,ldia,lhora] = dia;
	tBacklogFin[rt,dia,hr,ldia,lhora] = ldiafin-1;
	tNS[rt,dia,hr,ldia,lhora] = lns;
	tDiaSepFin[rt,dia,hr,ldia,lhora] = ldiafin;
	tHoraSepFin[rt,dia,hr,ldia,lhora] = lhorafin;
%mend Logistica_teste;
%macro Logistica_ns_util;
	/* Calcula o ns em dias uteis, isto eh, despreza horas de fim-de-semana e de feriados*/
	lnsu = 0;
	for{du in dia..(ldiafin)} do;
		if <cd,du> not in Feriado then do;
			for{ldi in slice(<cd,weekday(du),*>, CDDiaSemana): du >= ldi and du <= dataFinCDDiaSem[cd,weekday(du),ldi]} do;
				if tipoCDDiaSem[cd,weekday(du),ldi] = 'U' then do;
					if du = dia then do;
						if ldiafin > dia then lnsu = 24-hr;
						else lnsu = lhorafin-hr;
					end;
					else if du = ldiafin then lnsu = lnsu + lhorafin;
					else lnsu = lnsu + 24;
				end;
			end;
		end;
	end;
	if lnsu > &ns_max. then lnsu = &ns_max.;
/*	put cd= dia= date. ldiafin= date. hr= lhorafin= lns= lnsu=;*/
%mend Logistica_ns_util;
%macro Logistica_separa(TIPO);
/* Public Logistica.separa()*/
/* Variáveis de entrada: */
/*	ldia  = dia para início da separação*/
/*	lhora = hora para início da separação*/
/*	lDiaColeta = dia de coleta (opcional)*/
/*	lHoraColeta = hora de coleta (opcional)*/
/* Volume a ser separado em lresto*/

	lrestoini = lresto;
	do while (lresto > 0 and ldia <= lastDay+29);
		/* Se a capacidade estourou geral faz só o backlog*/
		lsep = 0;
		/* verifica se tem capacidade disponível*/
		if capCDHora[cd,ldia,lhora] >= lresto then do;
			lsep = lresto;
			lresto = 0;
		end;
		else if capCDHora[cd,ldia,lhora] > 0 then do;
			lresto = lresto - capCDHora[cd,ldia,lhora];
			lsep = capCDHora[cd,ldia,lhora];
		end;

		if lsep > 0 then do;
			/* Atualiza capacidades*/
			capCDHora[cd,ldia,lhora] = capCDHora[cd,ldia,lhora] - lsep;
			capCDDia[cd,ldia] = capCDDia[cd,ldia] - lsep;
			capCDSemana[cd,year(ldia),week(ldia)] = capCDSemana[cd,year(ldia),week(ldia)] - lsep;
			/* Acrescenta o tempo de separação e tempo da expedição do CD */
			/* Tem que terminar em algum turno ativo*/
			if lhora + tempoSepCD[cd] > 23 then do;
				ldiafin = ldia+1;
				lhorafin = lhora + tempoSepCD[cd] - 24;
			end;
			else do;
				ldiafin = ldia;
				lhorafin = lhora + tempoSepCD[cd];
			end;
			do while (capTotalCDHora[cd,ldiafin,lhorafin] = 0 and ldiafin <lastDay+30);
				if lhorafin + 1 > 23 then do;
					ldiafin = ldiafin+1;
					lhorafin = 0;
				end;
				else lhorafin = lhorafin+1;
			end;
			/* Volume separado no dia em terminou*/
			if <cd,dia,rt,ldiafin> not in sep&TIPO. then sep&TIPO. = sep&TIPO. union {<cd,dia,rt,ldiafin>};
			separa_&TIPO.[cd,dia,rt,ldiafin] = separa_&TIPO.[cd,dia,rt,ldiafin] + lsep;
			/* Nível de serviço*/
			lns = (ldiafin - dia)*24 + lhorafin - hr;
			if lns > &ns_max. then lns = &ns_max.;
			if <cd,dia,rt,lns,CNS_&TIPO.> not in NSSet then do;
				NSSet = NSSet union {<cd,dia,rt,lns,CNS_&TIPO.>};
				volNS[cd,dia,rt,lns,CNS_&TIPO.] = lsep;
			end;
			else
				volNS[cd,dia,rt,lns,CNS_&TIPO.] = volNS[cd,dia,rt,lns,CNS_&TIPO.] + lsep;
			/* Calcula ns util ==> devolve em lnsu*/
			%Logistica_ns_util;
			if <cd,dia,rt,lnsu,CNS_&TIPO.> not in NSUSet then do;
				NSUSet = NSUSet union {<cd,dia,rt,lnsu,CNS_&TIPO.>};
				volNSU[cd,dia,rt,lnsu,CNS_&TIPO.] = lsep;
			end;
			else
				volNSU[cd,dia,rt,lnsu,CNS_&TIPO.] = volNSU[cd,dia,rt,lnsu,CNS_&TIPO.] + lsep;
			/* verifica se vai atrasar a coleta*/
			/* Fazer Backlog por tipo de separação*/
			if ldiafin > dia then
				for{d in dia+1..(ldiafin)}
					backlog_&TIPO.[cd,d] = backlog_&TIPO.[cd,d] + lsep;
			%if &TIPO. ~= diario %then %do;
				if ldiafin > lDiaColeta or 
					(ldiafin = lDiaColeta and lhorafin > lHoraColeta) then do;
					if <cd,lDiaColeta,rt,CVIOL_&tipo.> not in AtrasaColetaSet then
						AtrasaColetaSet = AtrasaColetaSet union {<cd,lDiaColeta,rt,CVIOL_&tipo.>};
					volAtrasa[cd,lDiaColeta,rt,CVIOL_&tipo.] = volAtrasa[cd,lDiaColeta,rt,CVIOL_&tipo.] + lsep;
				end;
			%end;
/*			put '&TIPO. ' (put(dia,date.))= hr= (put(ldia,date.))= lhora= lsep= lns=;*/
			%if &teste. = 1 %then %do;
			if rt in teste_rt then do;
/*				put rt= ;*/
				%Logistica_teste;
			end;
			%end;
		end;
		/* Incrementa hora*/
		lhora = lhora + 1;
		if lhora > 23 then do;
			lhora = 0;
			ldia = ldia + 1;
		end;
	end;
	/* Não teve capacidade para separar ==> faz backlog*/
	if lresto > 0 then do;
		for{d in dia+1..(lastDay+30)}
			backlog_&TIPO.[cd,d] = backlog_&TIPO.[cd,d] + lresto;
	end;
%mend Logistica_separa;

%macro Logistica_demanda0;
/* Calcula a demanda0*/
/* Entrada cd,dia,st,zn*/
/*	put cd= dia= st= zn=;*/
	for{<gv,di1,df1> in gvSetor[st]: dia>=di1 and dia<=df1} lgv=gv;
	lre = reGV[lgv];
	lciclo = cicloGV[lgv,dia];
	lped = 0; lvol=0; litem = 0;
	if lciclo ~= 0 then do;
		lano = anoGV[lgv,dia];
		lestr = estrCiclo[lciclo,lano];
		ldiacc = dia - abreCiclo[lciclo,lano,lre,lgv] + 1;
		ldowcc = weekday(abreCiclo[lciclo,lano,lre,lgv]);
		/* Calcula número de pedidos no dia*/
		lped = demPedido[lciclo,lano,lre];
		lped = lped * percCNSetReg[st,zn];
		lped = lped * percCaptaDia[lciclo,lano,lre,lgv,ldowcc,ldiacc];
		lvol = lped * percRelDia[lciclo,lano,lre,lgv,ldowcc,ldiacc];
		litem = lped * percRelItemDia[lciclo,lano,lre,lgv,ldowcc,ldiacc];
%if &Backlog_Inicial. = 1 %then %do;
		if dia = firstDay and <st,zn> in BacklogSet then DO;
			lped = lped + pedidoBK[st,zn];
			lvol = lvol + volumeBK[st,zn];
		end;
%end;
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
	num diaCorte{rotaCALDia};
	num horaCorte{rotaCALDia};
	num diaColeta{rotaCALDia};
	num horaColeta{rotaCALDia};
	num calendDOWRota{rotaCALDia,1..7};
%mend Calendarizacao_init;
/* Private Calendarizacao.calcDataSep() - Calcula a data e hora de separacao*/
%macro Calendarizacao_calcDataSep;
/* Entra com a rota em crota, dia e hr ==> */
/* sai cDiaSep, cHoraSep e cDiaColeta*/
	cDiaSep = lastDay;
	ci = dia; cexit = 0;
	do while (ci <= dia+8 and cexit = 0);
/*		put dia = zn = i= cal_rota=;*/
		/* Essa não dá pra entender: se diaCorte = -1 verifica o dia seguinte*/
		if  calendDOWRota[crota,weekday(ci-cDiaCorte)] = 1 then do;
			if cHoraCorte < 23 then do;
				cDiaSep = ci;
				cHoraSep = cHoraCorte+1;
			end; else  do;
				cDiaSep = ci+1;
				cHoraSep = 0;
			end;
			cDiaColeta = ci-cDiaCorte + cColeta;
			/* Se estivermos no dia de corte após o horário de corte continue*/
			if ci ~= dia or hr < cHoraCorte then cexit = 1;
		end; 
		ci = ci + 1;
	end;

%mend Calendarizacao_calcDataSep;
%macro Calendarizacao_separa;
/*	Calcula as demandas CAL para o CD/Dia*/
	/* Faz conjunto de st,zn,rt calendarizados no CD/Dia atual*/
	rotaCALDia = setof{<zn,rt,di,df> in znRotaCAL[cd]: 
		dia>=di and dia<=df and <cd,dia,rt> in CDDiaRota} <rt>;
/* Inicializa todas as estrutURas para o CD/Dia corrente*/
	for{<rt,di> in CalendSet: rt in rotaCALDia and dia >= di and dia <= dataFinCAL[rt,di]} do;
		diaCorte[rt] = diaCorteCAL[rt,di];
		horaCorte[rt] = horaCorteCAL[rt,di];
		diaColeta[rt] = diaColetaCAL[rt,di];
		horaColeta[rt] = horaColetaCAL[rt,di];
		for{wd in 1..7}
			calendDOWRota[rt,wd] = calendDOW[rt,di,wd];
	end;
/* Separa por hora para não previlegiar nenhuma rt (separação calendarizada FIFO)*/
	for{hr in horaCapta} do;
		for{rt in rotaCALDia: volCALRota[cd,dia,rt] > 0} do;
/*			put zn= rt=;*/
			crota = rt;
			cDiaCorte = diaCorte[rt];
			cHoraCorte = hour(horaCorte[rt]);
			cColeta = diaColeta[rt];
			/* Calcula o próximo dia de separação calendarizada*/
			/* sai cDiaSep, cHoraSep e cDiaColeta*/
			%Calendarizacao_calcDataSep;
			if <cd,dia,rt,cDiaSep> not in progCalend then progCalend = progCalend union {<cd,dia,rt,cDiaSep>};
			volProg_calend[cd,dia,rt,cDiaSep] = volProg_calend[cd,dia,rt,cDiaSep] + 
				volCALRota[cd,dia,rt] * percCaptaHora[0,0,weekday(dia),'01JAN2000'd,hr];

/*			put (put(dia,date.))= hr= (put(cDiaSep,date.))=  cHoraSep=;*/
			/* Separa volumes calendarizados*/
			ldia = cDiaSep;
			lresto = volCALRota[cd,dia,rt] * percCaptaHora[0,0,weekday(dia),'01JAN2000'd,hr];
			/* Procura dia com capacidade de separação*/
			%Logistica_procura_cap;
			if lcapdia = ldia then do;
				lhora = cHoraSep;
			end;
			else do;
				ldia = lcapdia;
				lhora = 0;
			end;
/*			put ldia= lhora=;	*/
			lDiaColeta = cDiaColeta;
			lHoraColeta = hour(horaColeta[rt]);
			%Logistica_separa(calend);
		end;
	end;

%mend Calendarizacao_separa;


/******************** CLASSE EntregaExpressa *********************/
%macro EntregaExpressa_init;
/****************************** ENTREGA EXPRESSA *********************************/
/* Gera um conjunto de zoneamentos/dias de EE ativos e não ativos*/
	set<num,num,str,num> EESet = setof{<rt,di> in EntregaExpr, d in Dias, <rt1,zn,di1> in RotaZone, <cd,st,(zn),di2> in CDSetorZN: 
		rt=rt1 and d >= di and d <= dataFinEE[rt,di] and d>=di1 and d<=dataFinRZ[rt1,zn,di1] and
		st in setorZoneSet[zn] and d >= di2 and d <= dataFinCS[cd,st,zn,di2]} <cd,rt,zn,d>;
	num ligaEE{EESet} init 0;

/* Variáveis da classe*/
	/* Conjunto de rotas para um CD/dia (temporário)*/
	set rotaEE init {};
	num eedia, eevol;
%mend EntregaExpressa_init;
%macro EntregaExpressa_demanda;
	/* transfere a demanda diária para EE*/
	for{<rt,zn> in slice(<cd,*,*,dia>,EESet): ligaEE[cd,rt,zn,dia] = 1} do;
		volEERota[cd,dia,rt] = volEERota[cd,dia,rt] + volDiarioRota[cd,dia,rt];
		volDiarioRota[cd,dia,rt] = 0;
	end;
%mend EntregaExpressa_demanda;


%macro EntregaExpressa_separa(modo);
	/* Conjunto de st,zn de EE para o dia em questão*/
	rotaEE = setof{<rt,zn> in slice(<cd,*,*,dia>,EESet): ligaEE[cd,rt,zn,dia] = 1} <rt>;
	/* Faz separação dos volumes por hora de captação*/
	for{rt in rotaEE, di in slice(<rt,*>,EntregaExpr): dia >= di and dia <= dataFinEE[rt,di] and volEERota[cd,dia,rt] > 0} do;
		ldia = dia;
		%Logistica_procura_cap;
		eedia = lcapdia;
		eevol = volEERota[cd,dia,rt];
		for{hr in horaCapta} do;
			/* Separa volumes de entrega expressa*/
			ldia = eedia;
			/* Verifica se passou do horário de corte*/
			if hr >= hour(horaCorteEE[rt,di]) then do;
				/* Separa junto com o diário*/
				/* Verifica se estourou a capacidade do dia*/
				if ldia = dia then do;
					if hr = 23 then do;
						ldia = ldia + 1;
						lhora = 0;
					end;
					else lhora = hr + 1;
				end;
				else do;
					lhora = 0;
				end;
				/* Atualiza captação diária se o modo for separa diário*/
				%if &modo. = 'diario' %then %do;
				lresto = eevol * percCaptaHora[0,0,weekday(dia),'01JAN2000'd,hr];
				volEERota[cd,dia,rt] = volEERota[cd,dia,rt] - lresto;
				volDiarioRota[cd,dia,rt] = volDiarioRota[cd,dia,rt] + lresto;
				%Logistica_separa(diario);
				%end;
			end;
			/* Se não passou do horário de corte separa EE*/
			else do;
				/* Verifica se estourou a capacidade do dia*/
				if ldia = dia then do;
					lhora = hr + 1;
				end;
				else do;
					lhora = 0;
				end;
				lDiaColeta = dia + diaColetaEE[rt,di];
				lHoraColeta = hour(horaColetaEE[rt,di]);
				/* Atualiza captação EE se o modo for EE*/
				%if &modo. = 'EE' %then %do;
				lresto = eevol * percCaptaHora[0,0,weekday(dia),'01JAN2000'd,hr];
				%Logistica_separa(EE);
				%end;
			end;
		end;
	end;
%mend EntregaExpressa_separa;


/******************** CLASSE SepDiaria *********************/
%macro SepDiaria_init;
/* Variáveis de classe*/
	/* Conjunto de rotas por CD/dia (temporário)*/
	set rotaDiaria init {};
%mend SepDiaria_init;

%macro SepDiaria_separa;
	/* Faz separação dos volumes por hora de captação*/
	rotaCALDia = setof{<zn,rt,di,df> in znRotaCAL[cd]: 
		dia>=di and dia<=df} <rt>;
/*		dia>=di and dia<=df and <cd,dia,rt> in CDDiaRota} <rt>;*/
	rotaEE = setof{<rt,zn> in slice(<cd,*,*,dia>,EESet): ligaEE[cd,rt,zn,dia] = 1} <rt>;
	rotaDiaria = Rotas diff rotaCALDia diff rotaEE;
/*	put rotaDiaria=;*/
	for{rt in rotaDiaria: <cd,dia,rt> in CDDiaRota} do;	
		if volDiarioRota[cd,dia,rt] > 0 then do;
			ldia = dia;
			%Logistica_procura_cap;
			for{hr in horaCapta} do;
				/* Separa volumes diários*/
				/* Começa a separar 1 hr depois de coletar*/
				ldia = lcapdia;			
				if ldia = dia then do;	
					if hr = 23 then do;
						lhora = 0;
						ldia = ldia + 1;
					end;			
					else do;
						lhora = hr + 1;
					end;
				end;
				else lhora = 0;
				lresto = volDiarioRota[cd,dia,rt] * percCaptaHora[0,0,weekday(dia),'01JAN2000'd,hr];
				%Logistica_separa(diario);
			end;
		end;
	end;
%mend SepDiaria_separa;


%macro saida_padrao;
	num DEM_CAPTA_TOTAL{CDDia} init 0;
	num DEM_CAPTA_CAL{CDDia} init 0;
	num DEM_CAPTA_EE{CDDia} init 0;
	num DEM_CAPTA_DIARIA{CDDia} init 0;
	num CAPACIDADE{CDDia} init 0;
	num DEM_PROG_TOTAL{CDDia} init 0;
	num DEM_PROG_CAL{CDDia} init 0;
	num DEM_PROG_EE{CDDia} init 0;
	num DEM_PROG_DIARIA{CDDia} init 0;
	num SEP_CAL{CDDia} init 0;
	num SEP_EE{CDDia} init 0;
	num SEP_DIARIA{CDDia} init 0;
	num SEP_TOTAL{CDDia} init 0;
	num BACKLOG_TOTAL{CDDia} init 0;
	num NS_TOTAL{CDDia} init 0;
	num NS_DIARIO_EE{CDDia} init 0;
	num NS_CAL{CDDia} init 0;
	num NS_EE{CDDia} init 0;
	num NS_DIARIO{CDDia} init 0;
	num NSU_TOTAL{CDDia} init 0;
	num NSU_DIARIO_EE{CDDia} init 0;
	num NSU_CAL{CDDia} init 0;
	num NSU_EE{CDDia} init 0;
	num NSU_DIARIO{CDDia} init 0;
	num VIOLA_CAL{CDDia} init 0;
	num VIOLA_EE{CDDia} init 0;
	num sns_ee, svol_ee, sns_cal, svol_cal, sns_dia, svol_dia;

	for{<cdi,dia> in CDDia} do;
		for{rt in slice(<cdi,dia,*>,CDDiaRota): volDiarioRota[cdi,dia,rt] +
		volCALRota[cdi,dia,rt] + volEERota[cdi,dia,rt] > 0 } do;
			DEM_CAPTA_DIARIA[cdi,dia] = DEM_CAPTA_DIARIA[cdi,dia] + volDiarioRota[cdi,dia,rt];
			DEM_CAPTA_CAL[cdi,dia] = DEM_CAPTA_CAL[cdi,dia] + volCALRota[cdi,dia,rt];
			DEM_CAPTA_EE[cdi,dia] = DEM_CAPTA_EE[cdi,dia] + volEERota[cdi,dia,rt];
			if <cdi,dia,rt,CVIOL_calend> in AtrasaColetaSet then
				VIOLA_CAL[cdi,dia] = VIOLA_CAL[cdi,dia] + volAtrasa[cdi,dia,rt,CVIOL_calend];
			if <cdi,dia,rt,CVIOL_EE> in AtrasaColetaSet then
				VIOLA_EE[cdi,dia] = VIOLA_EE[cdi,dia] + volAtrasa[cdi,dia,rt,CVIOL_EE];
		end;
		DEM_CAPTA_TOTAL[cdi,dia] = DEM_CAPTA_TOTAL[cdi,dia] + DEM_CAPTA_DIARIA[cdi,dia] + DEM_CAPTA_CAL[cdi,dia] + DEM_CAPTA_EE[cdi,dia];
		/*Programação*/
		for{<rt,dprg> in slice(<cdi,dia,*,*>,progCalend)} do;
			DEM_PROG_CAL[cdi,dprg] = DEM_PROG_CAL[cdi,dprg] + volProg_calend[cdi,dia,rt,dprg];
		end;
		DEM_PROG_EE[cdi,dia] = DEM_CAPTA_EE[cdi,dia];
		DEM_PROG_DIARIA[cdi,dia] = DEM_CAPTA_DIARIA[cdi,dia];
		DEM_PROG_TOTAL[cdi,dia] = DEM_PROG_TOTAL[cdi,dia] + DEM_PROG_DIARIA[cdi,dia] + DEM_PROG_CAL[cdi,dia] + DEM_PROG_EE[cdi,dia];;

	/* Separação */
		for{<rt,dsep> in slice(<cdi,dia,*,*>,sepCalend)} do;
			SEP_CAL[cdi,dsep] = SEP_CAL[cdi,dsep] + separa_calend[cdi,dia,rt,dsep];
		end;
		for{<rt,dsep> in slice(<cdi,dia,*,*>,sepEE)} do;
			SEP_EE[cdi,dsep] = SEP_EE[cdi,dsep] + separa_EE[cdi,dia,rt,dsep];
		end;
		for{<rt,dsep> in slice(<cdi,dia,*,*>,sepDiario)} do;
			SEP_DIARIA[cdi,dsep] = SEP_DIARIA[cdi,dsep] + separa_diario[cdi,dia,rt,dsep];
		end;
		SEP_TOTAL[cdi,dia] = SEP_CAL[cdi,dia] + SEP_EE[cdi,dia] + SEP_DIARIA[cdi,dia];

		BACKLOG_TOTAL[cdi,dia] = backlog_calend[cdi,dia] + backlog_EE[cdi,dia] + backlog_diario[cdi,dia];
		/* NÍVEL DE SERVIÇO*/
		sns_cal = 0; svol_cal = 0; sns_ee=0; svol_ee=0; sns_dia=0; svol_dia=0;
		for{<rt,nsi,tipo> in slice(<cdi,dia,*,*,*>,NSSet)} do;
			if tipo = CNS_diario then do;
				sns_dia = sns_dia + volNS[cdi,dia,rt,nsi,tipo]*nsi;
				svol_dia = svol_dia + volNS[cdi,dia,rt,nsi,tipo];
			end;
			if tipo = CNS_calend then do;
				sns_cal = sns_cal + volNS[cdi,dia,rt,nsi,tipo]*nsi;
				svol_cal = svol_cal + volNS[cdi,dia,rt,nsi,tipo];
			end;
			if tipo = CNS_EE then do;
				sns_ee = sns_ee + volNS[cdi,dia,rt,nsi,tipo]*nsi;
				svol_ee = svol_ee + volNS[cdi,dia,rt,nsi,tipo];
			end;
		end;
		if svol_dia + svol_cal + svol_ee > 0 then 
			NS_TOTAL[cdi,dia] = (sns_ee+sns_cal+sns_dia)/ (svol_ee+svol_cal+svol_dia);
		if svol_dia + svol_ee > 0 then 
			NS_DIARIO_EE[cdi,dia] = (sns_ee+sns_dia)/ (svol_ee+svol_dia);
		if svol_dia > 0 then 
			NS_DIARIO[cdi,dia] = sns_dia / svol_dia;
		if svol_cal > 0 then 
			NS_CAL[cdi,dia] = sns_cal/svol_cal;
		if svol_ee > 0 then 
			NS_EE[cdi,dia] = sns_ee/svol_ee;

		/* NÍVEL DE SERVIÇO ÚTIL*/
		sns_cal = 0; svol_cal = 0; sns_ee=0; svol_ee=0; sns_dia=0; svol_dia=0;
		for{<rt,nsi,tipo> in slice(<cdi,dia,*,*,*>,NSUSet)} do;
			if tipo = CNS_diario then do;
				sns_dia = sns_dia + volNSU[cdi,dia,rt,nsi,tipo]*nsi;
				svol_dia = svol_dia + volNSU[cdi,dia,rt,nsi,tipo];
			end;
			if tipo = CNS_calend then do;
				sns_cal = sns_cal + volNSU[cdi,dia,rt,nsi,tipo]*nsi;
				svol_cal = svol_cal + volNSU[cdi,dia,rt,nsi,tipo];
			end;
			if tipo = CNS_EE then do;
				sns_ee = sns_ee + volNSU[cdi,dia,rt,nsi,tipo]*nsi;
				svol_ee = svol_ee + volNSU[cdi,dia,rt,nsi,tipo];
			end;
		end;
		if svol_dia + svol_cal + svol_ee > 0 then 
			NSU_TOTAL[cdi,dia] = (sns_ee+sns_cal+sns_dia)/ (svol_ee+svol_cal+svol_dia);
		if svol_dia + svol_ee > 0 then 
			NSU_DIARIO_EE[cdi,dia] = (sns_ee+sns_dia)/ (svol_ee+svol_dia);
		if svol_dia > 0 then 
			NSU_DIARIO[cdi,dia] = sns_dia / svol_dia;
		if svol_cal > 0 then 
			NSU_CAL[cdi,dia] = sns_cal/svol_cal;
		if svol_ee > 0 then 
			NSU_EE[cdi,dia] = sns_ee/svol_ee;

		for{hr in 0..23}
			CAPACIDADE[cdi,dia] = CAPACIDADE[cdi,dia] + capTotalCDHora[cdi,dia,hr];
	end;
	create data simula.saida_padrao3 from [COD_CD DATA]={<cd,data> in CDDia} MES=(month(data))
		DEM_CAPTA_TOTAL DEM_CAPTA_CAL DEM_CAPTA_EE DEM_CAPTA_DIARIA
		CAPACIDADE
		DEM_PROG_TOTAL DEM_PROG_CAL DEM_PROG_EE DEM_PROG_DIARIA
		SEP_CAL SEP_EE SEP_DIARIA SEP_TOTAL
		BACKLOG_CAL=backlog_calend BACKLOG_EE BACKLOG_DIARIO BACKLOG_TOTAL
		NS_TOTAL NS_DIARIO_EE NS_CAL NS_EE NS_DIARIO 
		NSU_TOTAL NSU_DIARIO_EE NSU_CAL NSU_EE NSU_DIARIO 
		VIOLA_CAL VIOLA_EE
	;


%mend saida_padrao;
%macro formata_saida_padrao;
data simula.saida_padrao3;
	format COD_CD 8. DATA date. MES 8.
		DEM_CAPTA_CAL COMMA12.2 DEM_CAPTA_EE COMMA12.2 DEM_CAPTA_DIARIA COMMA12.2 DEM_CAPTA_TOTAL COMMA12.2
		CAPACIDADE COMMA12.2
		DEM_PROG_CAL COMMA12.2 DEM_PROG_EE COMMA12.2 DEM_PROG_DIARIA COMMA12.2 DEM_PROG_TOTAL COMMA12.2 
		SEP_CAL COMMA12.2 SEP_EE COMMA12.2 SEP_DIARIA COMMA12.2 SEP_TOTAL COMMA12.2
		BACKLOG_CAL COMMA12.2 BACKLOG_EE COMMA12.2 BACKLOG_DIARIO COMMA12.2 BACKLOG_TOTAL COMMA12.2
		NS_CAL COMMA12.2 NS_EE COMMA12.2 NS_DIARIO COMMA12.2 NS_DIARIO_EE COMMA12.2 NS_TOTAL COMMA12.2 
		NSU_CAL COMMA12.2 NSU_EE COMMA12.2 NSU_DIARIO COMMA12.2 NSU_DIARIO_EE COMMA12.2 NSU_TOTAL COMMA12.2 
		VIOLA_CAL COMMA12.2 VIOLA_EE COMMA12.2
		;
	set simula.saida_padrao3;
run;

%mend formata_saida_padrao;
%macro saida_detalhada;
/* Dados detalhados para relatórios*/
	/*NS*/
	create data simula.saida_ns_detalhado3 from [COD_CD DATA ROTA NS TIPO_DEM]=
		{<cdi,dia,rt,ns,tipo> in NSSet: volNS[cdi,dia,rt,ns,tipo] > 0}
		VOLUME=volNS;
	/*NSU*/
	create data simula.saida_nsu_detalhado3 from [COD_CD DATA ROTA NS TIPO_DEM]=
		{<cdi,dia,rt,ns,tipo> in NSUSet: volNSU[cdi,dia,rt,ns,tipo] > 0}
		VOLUME=volNSU;
	/* Separação*/
	create data simula.saida_separacao3 from [COD_CD DATA_CAPTA ROTA DATA_SEPARA]=
		{<cdi,dia,rt,sep> in sepCalend union sepEE union sepDiario}
		SEPARA_CALEND SEPARA_EE SEPARA_DIARIO
	;
	/* Programação */
	create data simula.saida_programacao3 from [COD_CD DATA_CAPTA ROTA DATA_PROGRAMA]=progCalend 
		PROG_CALEND=volProg_calend;

%mend saida_detalhada;
%macro formata_saida_detalhada;
data simula.saida_ns_detalhado3;
	set simula.saida_ns_detalhado3;
	format DATA date. VOLUME comma12.2;
run;
data simula.saida_nsu_detalhado3;
	set simula.saida_nsu_detalhado3;
	format DATA date. VOLUME comma12.2;
run;
data simula.saida_separacao3;
	set simula.saida_separacao3;
	format DATA_CAPTA date. DATA_SEPARA date. 
	SEPARA_CALEND comma12.2
	SEPARA_EE comma12.2
	SEPARA_DIARIO comma12.2
	;
run;
data simula.saida_programacao3;
	set simula.saida_programacao3;
	format DATA_CAPTA date. DATA_PROGRAMA date. 
	PROG_CALEND comma12.2
	;
run;
%mend formata_saida_detalhada;
%macro saida_teste;
	create data teste from [ROTA DATA_CAPTA HORA_CAPTA DATA_SEP HORA_SEP]=TesteHora 
		VOLUME_CAPTA=tVolCapta VOLUME_SEP=tVolumeSepara NS=tNS DATA_COLETA=tDiaColeta HORA_COLETA=tHoraColeta
		DATA_INI_BACKLOG=tBacklogIni DATA_FIN_BACKLOG=tBacklogFin DATA_SEP_FIM=tDiaSepFin HORA_SEP_FIM=tHoraSepFin
	;
	create data caphora from [COD_CD DATA HORA]={<cdi,dia,hora> in CDDiaHora: cdi=5400 and 
		dia>=dia_ini_teste and dia<=dia_fin_teste} CAPACIDADE=capTotalCDHora CAP_OCIOSA=capCDHora;

	create data simula.saida_NS3 from [COD_CD DATA ROTA NS TIPO]=NSSet
		VOLUME=volNS
/*		CICLO=ciclo[cdi,dia,st,zn] ANO=ano[cdi,dia,st,zn] CD=cdi COD_PA=cod_pa[cdi,dia,st,zn]*/
/*		PA=pa[cdi,dia,st,zn] UR=ur[cdi,dia,st,zn] COD_RE=cod_re[cdi,dia,st,zn] RE=re[cdi,dia,st,zn]*/
/*		COD_GV=cod_gv[cdi,dia,st,zn] GV=gv[cdi,dia,st,zn] SETOR=setor[cdi,dia,st,zn] UF=uf[cdi,dia,st,zn] */
/*		NOME_UF=nome_uf[cdi,dia,st,zn] CIDADE=cidade[cdi,dia,st,zn] ROTA=rota[cdi,dia,st,zn] */
/*		TRANSPORTADORA=transportadora[cdi,dia,st,zn] FILIAL=filial[cdi,dia,st,zn] 	*/
	;
%mend saida_teste;

%macro formata_teste;
data teste;
	format rota 8. DATA_CAPTA date. HORA_CAPTA 8. VOLUME_CAPTA comma12.2 DATA_SEP date. HORA_SEP 8. DATA_SEP_FIM date. HORA_SEP_FIM 8. 
	VOLUME_SEP comma12.2 NS 8. DATA_COLETA date. HORA_COLETA 8. DATA_INI_BACKLOG date. DATA_FIN_BACKLOG date.;
	set teste;
run;
data caphora;
	set caphora;
	format data date.;
run;
data simula.saida_NS3;
	set simula.saida_NS3;
	format DATA date. VOLUME comma.;
run;

%mend formata_teste;

%macro leSaidaCaptacao;
PROC SQL;
   CREATE TABLE WORK.CD_DIA_ROTA AS 
   SELECT t1.COD_CD, 
          t1.DATA, 
          t1.ROTA, 
          /* VOLUME_CALEND */
            (SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_CALEND, 
          /* VOLUME_DIARIO */
            (SUM(t1.VOLUME_DIARIO)) FORMAT=COMMA12.2 AS VOLUME_DIARIO 
      FROM SIMULA.SAIDA_CAPTACAO3 t1
      GROUP BY t1.COD_CD,
               t1.DATA,
               t1.ROTA;
QUIT;
/* Acrescenta dados de backlog (não contabiliza itens, mas o anterior tampouco*/
%if &Backlog_Inicial. = 1 %then %do;
	PROC SQL;
		CREATE TABLE WORK.BACKLOG_00 AS 
			SELECT T3.COD_CD, 
				T1.COD_SETOR, 
				T2.ROTA, 
				T1.ZONEAMENTO, 
				T1.PEDIDO, 
				T1.VOLUME, 
				T1.VALOR
			FROM SIMULA.BACKLOG T1
				INNER JOIN WORK.ROTA_ZONEAMENTO T2 ON (T1.ZONEAMENTO = T2.ZONEAMENTO)
				INNER JOIN WORK.CD_SETOR_ZONEAMENTO T3 ON (T1.COD_SETOR = T3.COD_SETOR);
	QUIT;
	PROC SQL NOPRINT;
	   SELECT (MIN(t1.DATA)) FORMAT=DATE. INTO :bl_data_ini
	      FROM WORK.CD_DIA_ROTA t1;
	QUIT;
	%put bl_data_ini = &bl_data_ini;
	PROC SQL;
		CREATE TABLE WORK.BACKLOG_01 AS 
			SELECT t1.COD_CD, 
				t1.COD_SETOR, 
				t1.ROTA, 
				t1.ZONEAMENTO, 
				(t1.VOLUME * (t2.ROTA = .)) AS VOLUME_DIARIO, 
				(t1.VOLUME * (t2.ROTA ~= .)) AS VOLUME_CALEND
			FROM WORK.BACKLOG_00 t1
				LEFT JOIN SIMULA.CALENDARIZACAO t2 ON (t1.ROTA = t2.ROTA);
	QUIT;
	PROC SQL;
		CREATE TABLE WORK.BACKLOG_02 AS 
			SELECT t1.COD_CD, 
				"&bl_data_ini"d FORMAT=DATE. AS DATA, 
				t1.ROTA, 
				(SUM(t1.VOLUME_DIARIO)) AS VOLUME_DIARIO, 
				(SUM(t1.VOLUME_CALEND)) AS VOLUME_CALEND
			FROM WORK.BACKLOG_01 t1
				GROUP BY t1.COD_CD,
					t1.ROTA;
	QUIT;
	/*SOMA AO CD_DIA_ROTA*/
	DATA CD_DIA_ROTA_00;
		SET CD_DIA_ROTA BACKLOG_02;
	RUN;
	PROC SQL;
		CREATE TABLE CD_DIA_ROTA AS 
			SELECT t1.COD_CD, 
				t1.DATA, 
				t1.ROTA, 
				(SUM(t1.VOLUME_CALEND)) FORMAT=COMMA12.2 AS VOLUME_CALEND, 
				(SUM(t1.VOLUME_DIARIO)) FORMAT=COMMA12.2 AS VOLUME_DIARIO
			FROM WORK.CD_DIA_ROTA_00 t1
				GROUP BY t1.COD_CD,
					t1.DATA,
					t1.ROTA;
	QUIT;
%end;
%mend leSaidaCaptacao;

/******************** Main *********************/
%macro simula;
%leSaidaCaptacao
proc optmodel;
	num current init 0;
	%timing;
	put 'INÍCIO';
	%leDados;
	/* Inicialização das classes*/
	%timing;
	put 'LEDADOS FIM';
	%Comercial_init;
	%timing;
	put 'INICIALIZAÇÃO COMERCIAL FIM';
	%Logistica_init;
	%timing;
	put 'INICIALIZAÇÃO LOGÍSTICA FIM';
	%Calendarizacao_init;
	%timing;
	put 'INICIALIZAÇÃO CALENDARIZAÇÃO FIM';
	%EntregaExpressa_init;
	%timing;
	put 'INICIALIZAÇÃO EE FIM';
	%SepDiaria_init;

	put 'INICIALIZAÇÃO FIM';
	%timing;

	for{cd in CDSet} do;
		for{dia in Dias} do;
			/* Separa volumes calendarizados*/
			%Calendarizacao_separa;
		end;
		put 'CALENDARIZAÇÃO FIM';
		%timing;

		for{dia in Dias} do;
			put cd= dia=date.;
			/* Separa volumes diários*/
			%SepDiaria_separa;
		end;
		put 'DIÁRIO FIM';	
	end;

%if &Saida_Padrao. = 1 %then %do;
	%timing;
	%saida_padrao;
	put 'SAÍDA PADRÃO FIM';
%end;
	%timing;
	%saida_detalhada;
	put 'SAÍDA DETALHADA FIM';
	%timing;
%if &teste. = 1 %then %do;
	%saida_teste;
%end;
quit;

%if &Saida_Padrao. = 1 %then %do;
%formata_saida_padrao;
%end;
%formata_saida_detalhada;
%if &teste. = 1 %then %do;
	%formata_teste;
%end;
%mend simula;
%macro relatorio_ns;
/* Transforma o input de volume_tipo em volue/tipo*/
data saida_ns_000(drop = volume_calend volume_ee volume_diario pedido_calend pedido_ee pedido_diario);
	set SIMULA.SAIDA_CAPTACAO3;
	format TIPO $6.;
	if volume_calend > 0 then do;
		VOLUME = volume_calend;
		PEDIDO = pedido_calend;
		TIPO = 'CAL';
		output;
	end;
	if volume_ee > 0 then do;
		VOLUME = volume_ee;
		PEDIDO = pedido_ee;
		TIPO = 'EE';
		output;
	end;
	if volume_diario > 0 then do;
		VOLUME = volume_diario;
		PEDIDO = pedido_diario;
		TIPO = 'DIARIO';
		output;
	end;
run;
/* Acha o volume do setor na rota/tipo*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_NS_001 AS SELECT
		t1.ANO,
		t1.CICLO, 
		t1.COD_CD, 
		t1.DATA,
		t1.GV,
		t1.COD_SETOR, 
		t1.ROTA,
		t1.TIPO, 
		/* SUM_of_VOLUME */
		(SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS SUM_of_VOLUME
	FROM WORK.SAIDA_NS_000 AS t1
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.DATA, t1.GV, t1.COD_SETOR, t1.ROTA, t1.TIPO;
QUIT;
/* Cria o volume captado por rota em simula.saida_ns_volrota*/
proc sql;
	CREATE TABLE simula.SAIDA_NS_VOLROTA3 AS SELECT 
		t1.COD_CD, 
		t1.DATA,
		t1.TIPO,
		t1.ROTA, 
		/* SUM_of_VOLUME */
		(SUM(t1.VOLUME)) FORMAT=COMMA12.2 AS VOLUME
	FROM WORK.SAIDA_NS_000 AS t1	
	GROUP BY t1.COD_CD, t1.DATA, t1.ROTA, t1.TIPO;
quit;
/* Junta com o volume captado da rota/tipo para calcular a participação do setor na rota/tipo*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_NS_002 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.DATA, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ROTA,
		t1.TIPO, 
		/* PART_SETOR_ROTA */
		(t1.SUM_of_VOLUME/t2.VOLUME) AS PART_SETOR_ROTA
	FROM WORK.SAIDA_NS_001 AS t1, SIMULA.SAIDA_NS_VOLROTA3 AS t2
	WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA AND t1.TIPO=t2.TIPO)
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.DATA, t1.ROTA, t1.GV, t1.COD_SETOR;
QUIT;
/* Junta com NS usando o valor da participação do setor na rota*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_NS_003 AS SELECT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.DATA, 
		t1.GV AS COD_GV,
		t1.COD_SETOR,
		t2.NS, 
		t2.TIPO_DEM, 
		/* VOLUME */
		sum(t2.VOLUME*t1.PART_SETOR_ROTA) AS VOLUME
	FROM WORK.SAIDA_NS_002 AS t1, SIMULA.SAIDA_NS_DETALHADO3 AS t2
	WHERE (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA AND t1.ROTA = t2.ROTA AND t1.TIPO=t2.TIPO_DEM)
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.DATA, t1.GV, t1.COD_SETOR, t2.TIPO_DEM, t2.NS 
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.DATA, t1.GV, t1.COD_SETOR, t2.TIPO_DEM, t2.NS 
	;
QUIT;
/* Completa com os campos faltantes em estrutura comercial*/
PROC SQL;
	CREATE TABLE SAIDA_NS_004 AS SELECT DISTINCT
		UR,
		COD_RE,
		RE,
		COD_GV,
		GV,
		COD_SETOR,
		SETOR
	FROM WORK.ESTRUTURA_COMERCIAL;
QUIT;
PROC SQL;
	CREATE TABLE SIMULA.SAIDA_NS_SETOR_DETALHADO3 AS SELECT DISTINCT 
		t1.COD_CD, 
		t3.CD,
		t1.DATA, 
		t1.ANO,
		t1.CICLO, 
		t2.UR, 
		t2.COD_RE, 
		t2.RE, 
		t1.COD_GV, 
		t2.GV, 
		t1.COD_SETOR, 
		t2.SETOR, 
		t1.TIPO_DEM, 
		t1.NS, 
		t1.VOLUME
	FROM WORK.SAIDA_NS_003 AS t1
	inner join SAIDA_NS_004 AS t2 
		on t1.COD_GV=t2.COD_GV AND t1.COD_SETOR = t2.COD_SETOR
	inner join  CD AS t3 on t1.COD_CD = t3.COD_CD
	;
QUIT;

%mend relatorio_ns;
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
          t1.PEDIDO_CAPTADO,
            (t1.ITEM_PROGRAMADO * t2.FATOR_CONC) AS ITEM_PROGRAMADO, 
		  t1.VOLUME_PROGRAMADO,
		  t1.PEDIDO_PROGRAMADO,
            (t1.ITEM_SEPARADO * t2.FATOR_CONC) AS ITEM_SEPARADO, 
		  t1.VOLUME_SEPARADO,
		  t1.PEDIDO_SEPARADO      
	  FROM WORK.SAIDA_DEM1_001 t1, WORK.RECONC_ITEM t2
      WHERE (t1.ANO = t2.ANO AND t1.CICLO = t2.CICLO AND t1.RE = t2.RE);
QUIT;
%mend rdd_concilia_item;

%macro rel_demanda_detalhada;
/* Transforma o input de volume_tipo em volue/tipo*/
data SAIDA_CAPTA_000(drop = item_calend item_diario volume_calend volume_diario pedido_calend pedido_diario);
	set SIMULA.SAIDA_CAPTACAO3;
	format TIPO_DEM $6.;
	if volume_calend > 0 then do;
		ITEM = item_calend;
		VOLUME = volume_calend;
		PEDIDO = pedido_calend;
		TIPO_DEM = 'CAL';
		output;
	end;
	if volume_ee > 0 then do;
		VOLUME = volume_ee;
		PEDIDO = pedido_ee;
		TIPO_DEM = 'EE';
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
/* Agora é que é: vamos tratar a demanda programada*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_PROG_001 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		/* PART_SETOR_ROTA */
		t2.DATA_PROGRAMA AS DATA,
		sum(t1.PART_CAPTA_ROTA*t2.PROG_CALEND*t1.ITEM_CAPTADO/t1.VOLUME_CAPTADO) AS ITEM_PROGRAMADO,
		sum(t1.PART_CAPTA_ROTA*t2.PROG_CALEND) AS VOLUME_PROGRAMADO,
		sum(t1.PART_CAPTA_ROTA*t2.PROG_CALEND*t1.PEDIDO_CAPTADO/t1.VOLUME_CAPTADO) AS PEDIDO_PROGRAMADO
	FROM WORK.SAIDA_CAPTA_002 AS t1
	INNER JOIN SIMULA.SAIDA_PROGRAMACAO3 AS t2 
		ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA_CAPTA AND t1.ROTA = t2.ROTA)
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO,t1.ROTA,
		t1.TIPO_DEM, t2.DATA_PROGRAMA
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO, t1.ROTA, t1.TIPO_DEM, t2.DATA_PROGRAMA
	;
QUIT;
/* Demanda programada para EE e diario é no mesmo dia*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_PROG_002 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		/* PART_SETOR_ROTA */
		t1.DATA,
		t1.ITEM_CAPTADO,
		t1.VOLUME_CAPTADO,
		t1.PEDIDO_CAPTADO,
		CASE
			WHEN t1.TIPO_DEM IN ('EE','DIARIO') THEN t1.ITEM_CAPTADO 
			ELSE 0
		END AS ITEM_PROGRAMADO,
		CASE
			WHEN t1.TIPO_DEM IN ('EE','DIARIO') THEN t1.VOLUME_CAPTADO 
			ELSE 0
		END AS VOLUME_PROGRAMADO,
		CASE
			WHEN t1.TIPO_DEM IN ('EE','DIARIO') THEN t1.PEDIDO_CAPTADO 
			ELSE 0
		END AS PEDIDO_PROGRAMADO
	FROM WORK.SAIDA_CAPTA_002 AS t1 
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO, t1.ROTA, t1.TIPO_DEM, t1.DATA
	;
QUIT;
/* Junta tudo*/

DATA SAIDA_PROG;
	MERGE SAIDA_PROG_002 SAIDA_PROG_001;
	BY ANO CICLO COD_CD GV COD_SETOR ZONEAMENTO ROTA TIPO_DEM DATA;
RUN;
/* Agora a separada*/
/* CAL*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_SEP_001 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		t2.DATA_SEPARA AS DATA,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_CALEND*t1.ITEM_CAPTADO/t1.VOLUME_CAPTADO) AS ITEM_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_CALEND) AS VOLUME_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_CALEND*t1.PEDIDO_CAPTADO/t1.VOLUME_CAPTADO) AS PEDIDO_SEPARADO
	FROM WORK.SAIDA_CAPTA_002 AS t1
	INNER JOIN SIMULA.SAIDA_SEPARACAO3 AS t2 
		ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA_CAPTA AND t1.ROTA = t2.ROTA AND TIPO_DEM='CAL')
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO,t1.ROTA,
		t1.TIPO_DEM, t2.DATA_SEPARA
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO, t1.ROTA, t1.TIPO_DEM, t2.DATA_SEPARA
	;
QUIT;
/* ENTREGA EXPRESSA*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_SEP_002 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		t2.DATA_SEPARA AS DATA,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_EE*t1.ITEM_CAPTADO/t1.VOLUME_CAPTADO) AS ITEM_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_EE) AS VOLUME_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_EE*t1.PEDIDO_CAPTADO/t1.VOLUME_CAPTADO) AS PEDIDO_SEPARADO
	FROM WORK.SAIDA_CAPTA_002 AS t1
	INNER JOIN SIMULA.SAIDA_SEPARACAO3 AS t2 
		ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA_CAPTA AND t1.ROTA = t2.ROTA AND TIPO_DEM='EE')
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO,t1.ROTA,
		t1.TIPO_DEM, t2.DATA_SEPARA
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO, t1.ROTA, t1.TIPO_DEM, t2.DATA_SEPARA
	;
QUIT;
/* DIARIO*/
PROC SQL;
	CREATE TABLE WORK.SAIDA_SEP_003 AS SELECT DISTINCT 
		t1.ANO,
		t1.CICLO,
		t1.COD_CD, 
		t1.GV,
		t1.COD_SETOR, 
		t1.ZONEAMENTO,
		t1.ROTA,
		t1.TIPO_DEM, 
		t2.DATA_SEPARA AS DATA,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_DIARIO*t1.ITEM_CAPTADO/t1.VOLUME_CAPTADO) AS ITEM_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_DIARIO) AS VOLUME_SEPARADO,
		sum(t1.PART_CAPTA_ROTA*t2.SEPARA_DIARIO*t1.PEDIDO_CAPTADO/t1.VOLUME_CAPTADO) AS PEDIDO_SEPARADO
	FROM WORK.SAIDA_CAPTA_002 AS t1
	INNER JOIN SIMULA.SAIDA_SEPARACAO3 AS t2 
		ON (t1.COD_CD = t2.COD_CD AND t1.DATA = t2.DATA_CAPTA AND t1.ROTA = t2.ROTA AND TIPO_DEM='DIARIO')
	GROUP BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO,t1.ROTA,
		t1.TIPO_DEM, t2.DATA_SEPARA
	ORDER BY t1.ANO, t1.CICLO, t1.COD_CD, t1.GV, t1.COD_SETOR, t1.ZONEAMENTO, t1.ROTA, t1.TIPO_DEM, t2.DATA_SEPARA
	;
QUIT;
/* Junta separação*/
DATA SAIDA_SEP;
	SET  SAIDA_SEP_001 SAIDA_SEP_002 SAIDA_SEP_003;
RUN;
PROC SORT DATA=SAIDA_SEP;
	BY ANO CICLO COD_CD GV COD_SETOR ZONEAMENTO ROTA TIPO_DEM DATA;
QUIT;
/* Junta captação programação e separação*/
DATA SAIDA_TOTAL;
	MERGE SAIDA_PROG SAIDA_SEP;
	BY ANO CICLO COD_CD GV COD_SETOR ZONEAMENTO ROTA TIPO_DEM DATA;
RUN;

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
		t1.PEDIDO_CAPTADO,
		t1.ITEM_PROGRAMADO,
		t1.VOLUME_PROGRAMADO,
		t1.PEDIDO_PROGRAMADO,
		t1.ITEM_SEPARADO,
		t1.VOLUME_SEPARADO,
		t1.PEDIDO_SEPARADO
	FROM SAIDA_TOTAL AS t1
	inner join WORK.ESTRUTURA_COMERCIAL AS t2 
		on t1.GV=t2.COD_GV AND t1.COD_SETOR = t2.COD_SETOR AND t1.ZONEAMENTO=t2.ZONEAMENTO
	inner join CD AS t3 on t1.COD_CD = t3.COD_CD
	;
QUIT;
/*Reconcilia item captado de acordo com a demanda entrada=saida_dem1_001 saida=saida_dem1_002*/
%rdd_concilia_item

/* Junta com informações de PA e Transportadora.*/
PROC SQL;
	CREATE TABLE SIMULA.SAIDA_DEMANDA_DETALHADA3 AS SELECT DISTINCT 
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
%init_nova_estrutura

%if &Rodar_Simulacao. = 1 %then %do;
%simula;
%end;
%if &NS_Detalhado. = 1 %then %do;
%relatorio_ns;
%end;
%if &Demanda_Detalhada. = 1 %then %do;
%rel_demanda_detalhada;
%end;
/* LIMPA O LIXO*/
proc datasets library=work kill memtype=data nolist;
quit;
%mend main;
%main;

