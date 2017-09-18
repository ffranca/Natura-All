%macro leDados;
/******************************* CARREGA TABELAS SIMULA ***************************/
/* Carrega DEMANDA_PEDIDOS*/
	set<num,num> DemandaSet;
	set nRegDemanda = 1..8;
	str RegDemanda{nRegDemanda} = ['SUL','SP CAPITAL','RJ-ES','MG','SP INTERIOR','CENTRO OESTE','NORDESTE','NORTE'];
	num demPedido{DemandaSet,nRegDemanda};
	read data simula.DEMANDA_PEDIDOS into DemandaSet=[CICLO ANO]
		{reg in nRegDemanda} <demPedido[ciclo,ano,reg] = col(RegDemanda[reg])>;
/*	print demPedido;*/

/* Carrega DEMANDA_R$*/
/*	num demReais{DemandaSet,nRegDemanda};*/
/*	read data simula.DEMANDA_RS into DemandaSet=[CICLO ANO]*/
/*		{reg in nRegDemanda} <demReais[ciclo,ano,reg] = col(RegDemanda[reg])>;*/
/*	print demReais;*/

/* Carrega ESTRUTURA_COMERCIAL*/
	set<num,str,num> EstrComSet;
	str urEC{EstrComSet};
	num reEC{EstrComSet};
	str reNomeEC{EstrComSet};
	num gvEC{EstrComSet};
	str gvNomeEC{EstrComSet};
	str setorNomeEC{EstrComSet};
	str ufEC{EstrComSet};
	str ufNomeEC{EstrComSet};
	str cidadeEC{EstrComSet};
	num qtdCN{EstrComSet};
	num dataFinEC{EstrComSet};
	read data simula.ESTRUTURA_COMERCIAL into EstrComSet=[COD_SETOR ZONEAMENTO DATA_INI]
		urEC=UR reEC=COD_RE reNomeEC=RE gvEC=COD_GV gvNomeEC=GV setorNomeEC=SETOR ufEC=UF 
		ufNomeEC=NOME_UF cidadeEC=CIDADE qtdCN=QTD_CN dataFinEC=DATA_FIN;
/*	print qtdCN;*/

/* Carrega CD*/
	set CDSet init {};
	str CDNome{CDSet};
	num dataIniCD{CDSet};
	num dataFinCD{CDSet};
	read data simula.CD into CDSet=[COD_CD] dataIniCD=DATA_INI CDNome=CD dataFinCD=DATA_FIN;
/*	read data simula.CD(where=(DATA_INI>"01jan2010"d)) into CDSet=[COD_CD] dataIniCD=DATA_INI CDNome=CD dataFinCD=DATA_FIN;*/
/*	print CDNome dataIniCD dataFinCD;*/

/* Carrega PA*/
	set PASet;
	str PANome{PASet};
	num dataIniPA{PASet};
	num dataFinPA{PASet};
	read data simula.PA into PASet=[COD_PA] dataIniPA=DATA_INI PANome=PA dataFinPA=DATA_FIN;
/*	print PANome dataIniPA dataFinPA;*/

/* Carrega CD_SETOR*/
	set<num,num,num> CDSetor;
	num dataFinCS{CDSetor};
	read data simula.CD_SETOR into CDSetor=[COD_CD COD_SETOR DATA_INI] dataFinCS=DATA_FIN;
/*	print dataFinCS;*/

/* Carrega PA_ROTA*/
	set<num,num,num> PARota;
	num dataFinPR{PARota};
	read data simula.PA_ROTA into PARota=[COD_PA ROTA DATA_INI] dataFinPR=DATA_FIN;
/*	print dataFinPR;*/

/* Carrega ROTA_ZONEAMENTO*/
	set<num,str,num> RotaZone;
	num dataFinRZ{RotaZone};
	read data simula.ROTA_ZONEAMENTO into RotaZone=[ROTA ZONEAMENTO DATA_INI] dataFinRZ=DATA_FIN;
/*	print dataFinRZ;*/

/* Carrega ROTA_TRANSPORTADORA_FILIAL */
	set<str,str,num,num> RotaTransp;
	num dataFinRT{RotaTransp};
	read data simula.ROTA_TRANSPORTADORA_FILIAL into RotaTransp=[TRANSPORTADORA FILIAL ROTA DATA_INI] dataFinRT=DATA_FIN;
/*	print dataFinRT;*/

/* Carrega CAPTACAO_DIA*/
	set<num,num,num,num,num> CaptaDiaSet;
	set diaCapta = {1..28};
	num percCaptaDia{CaptaDiaSet,diaCapta};
	read data simula.CAPTACAO_DIA into CaptaDiaSet=[CICLO ANO COD_RE COD_GV DIA_SEMANA]
	{dia in diaCapta} <percCaptaDia[ciclo,ano,cod_re,cod_gv,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percCaptaDia;*/
/*	for{<cc,ano,re,gv,cv> in CaptaDiaSet}*/
/*		put percCaptaDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega RELACAO_DIA*/
	set<num,num,num,num,num> RelDiaSet;
	set diaRel = {1..28};
	num percRelDia{RelDiaSet,diaRel};
	read data simula.RELACAO_DIA into RelDiaSet=[CICLO ANO COD_RE COD_GV DIA_SEMANA]
	{dia in diaRel} <percRelDia[ciclo,ano,cod_re,cod_gv,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percRelDia;*/
/*	for{<cc,ano,re,gv,cv> in RelDiaSet}*/
/*		put percRelDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega RELACAO_ITEM_DIA*/
	set<num,num,num,num,num> RelItemDiaSet;
	num percRelItemDia{RelItemDiaSet,diaRel};
	read data simula.RELACAO_ITEM_DIA into RelItemDiaSet=[CICLO ANO COD_RE COD_GV DIA_SEMANA]
	{dia in diaRel} <percRelItemDia[ciclo,ano,cod_re,cod_gv,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percRelItemDia;*/
/*	for{<cc,ano,re,gv,cv> in RelItemDiaSet}*/
/*		put percRelItemDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega CALENDARIO*/
	set<num,num,num,num> Calendario;
	num abreCiclo{Calendario};
	num fechaCiclo{Calendario};
	str estrategiaCal{Calendario};
	read data simula.CALENDARIO into Calendario=[CICLO ANO COD_RE COD_GV]
		abreCiclo=ABERTURA fechaCiclo=FECHAMENTO estrategiaCal=ESTRATEGIA;
/*	print abreCiclo fechaCiclo;*/

/* Carrega CAPTACAO_HORA*/
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
	read data simula.CALENDARIZACAO into CalendSet=[ROTA DATA_INICIO] 
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


/* Carrega GRUPO_ROTA*/
	set<num,num,num> GrupoRotaSet;
	num dataFinGR{GrupoRotaSet};
	read data simula.GRUPO_ROTA into GrupoRotaSet=[ROTA GRUPO_ROTA DATA_INI] dataFinGR=DATA_FIN;
/*	print dataFinGR;*/

/* Carrega CAP_GRUPO_ROTA*/
	set<num,num> CapGrupoRotaSet;
	num capacidadeCG{CapGrupoRotaSet};
	num dataFinCG{CapGrupoRotaSet};
	read data simula.CAP_GRUPO_ROTA into CapGrupoRotaSet=[GRUPO_ROTA DATA_INI]
		capacidadeCG=CAPACIDADE_DIA dataFinCG=DATA_FIN;
/*	print capacidadeCG dataFinCG;*/

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
/*proc optmodel;*/
/*	%leDados;*/
/*quit;*/
