%macro leDadosCaptacao;
/******************************* CARREGA TABELAS SIMULA ***************************/
/* Carrega DEMANDA_PEDIDOS*/
	set<num,num> DemandaSet;
	set nRegDemanda = 1..8;
	str RegDemanda{nRegDemanda} = ['SUL','SP CAPITAL','RJ-ES','MG','SP INTERIOR','CENTRO OESTE','NORDESTE','NORTE'];
	num demPedido{DemandaSet,nRegDemanda};
	read data simula.DEMANDA_PEDIDOS into DemandaSet=[CICLO ANO]
		{reg in nRegDemanda} <demPedido[ciclo,ano,reg] = col(RegDemanda[reg])>;
/*	print demPedido;*/

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
	num maxDiasCiclo = 35;
	set<num,num,num,num,num> CaptaDiaSet;
	set<num> diaCapta;
	diaCapta = {1..maxDiasCiclo};
	num percCaptaDia{CaptaDiaSet,diaCapta};
	read data simula.CAPTACAO_DIA into CaptaDiaSet=[CICLO ANO COD_RE COD_GV DIA_SEMANA]
	{dia in diaCapta} <percCaptaDia[ciclo,ano,cod_re,cod_gv,dia_semana,dia] = col(compress(put(dia,8.0)))>;
/*	print percCaptaDia;*/
/*	for{<cc,ano,re,gv,cv> in CaptaDiaSet}*/
/*		put percCaptaDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega CPT_PESO_DIAS_CICLO*/
	set<num> PesoDiasCicloSet;
	num percVarDiasCiclo{PesoDiasCicloSet};
	read data simula.CPT_PESO_DIAS_CICLO into PesoDiasCicloSet=[DIAS_CICLO] percVarDiasCiclo=VARIACAO_DEMANDA;
/*	print percVarDiasCiclo;*/

/* Carrega RELACAO_DIA*/
	set<num,num,num,num,num> RelDiaSet;
	set<num> diaRel;
	diaRel = {1..maxDiasCiclo};
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
/*	print percRelDia;*/
/*	for{<cc,ano,re,gv,cv> in RelItemDiaSet}*/
/*		put percRelDia[cc,ano,re,gv,cv,1]=;*/

/* Carrega CALENDARIO*/
	set<num,num,num,num> Calendario;
	num abreCiclo{Calendario};
	num abreCicloOrg{Calendario};
	num fechaCiclo{Calendario};
	str estrategiaCal{Calendario};
	read data simula.CALENDARIO into Calendario=[CICLO ANO COD_RE COD_GV]
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
	read data simula.CALENDARIZACAO into CalendSet=[ROTA DATA_INICIO] 
		horaCorteCAL=HORA_CORTE diaCorteCAL=DIA_CORTE dataFinCAL=DATA_FINAL
		diaColetaCAL=DIA_COLETA horaColetaCAL=HORA_COLETA
		{i in 1..7} <tempCalendDOW[rota,data_inicio,i] = col(substr(DOW[i],1,3))>;
	for{<z,di> in CalendSet, d in 1..7}
		if tempCalendDOW[z,di,d] ~= '' then calendDOW[z,di,d] = 1;
/*	print calendDOW;*/

/* Carrega FERIADO*/
	set<num,num> Feriado;
	num folgaNoturno{Feriado};
	read data simula.FERIADO(where=(tipo='F')) into Feriado=[COD_CD DATA] folgaNoturno=FOLGA_NOTURNO;
/*	print folgaNoturno tipoFeriado;*/

%mend leDadosCaptacao;