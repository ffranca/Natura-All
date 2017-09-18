LIBNAME simula BASE "D:\SASCONFIG\Lev1\SASApp\Data\natura\PLANEJAMENTO_DSC";

proc optmodel;
/*Lê ROTA_ZONEAMENTO*/
	set<num,str,num,num> RotaZNSet;
	num prazo_contratado{RotaZNSet};
	read data simula.rota_zoneamento into RotaZNSet=[ROTA ZONEAMENTO DATA_INI DATA_FIN] prazo_contratado;
/*Lê DATA_ALVO*/
	set DataAlvoSet;
	read data simula.DATA_ALVO into DataAlvoSet=[DATA_ALVO];
/*Lê CD_SETOR*/
	set<num,num,num,num> CDSetorSet;
	read data simula.CD_SETOR into CDSetorSet=[COD_CD COD_SETOR DATA_INI DATA_FIN];
/*Lê ESTRUTURA_COMERCIAL*/
	set<num,str,num,num> EComSet;
	str UR{EComSet};
	num COD_RE{EComSet};
	str RE{EComSet};
	num COD_GV{EComSet};
	str GV{EComSet};
	str SETOR{EComSet};
	num QTD_CN{EComSet};
	read data simula.ESTRUTURA_COMERCIAL into EComSet=[COD_SETOR ZONEAMENTO DATA_INI DATA_FIN] UR COD_RE COD_GV RE GV SETOR QTD_CN;
/*Lê FERIADO*/
	set<num,num> FeriadoSet;
	read data simula.FERIADO(where=(TIPO='F')) into FeriadoSet=[COD_CD DATA];

/*Lê CALENDARIZACAO*/
	set<num,num,num> CalendSet;
	str DOW{1..7} = ['DOM' 'SEG' 'TER' 'QUA' 'QUI' 'SEX' 'SAB'];
	str tempCalendDOW{CalendSet,1..7};
	num calendDOW{CalendSet,1..7} init 0;
	read data simula.CALENDARIZACAO into CalendSet=[ROTA DATA_INICIO DATA_FINAL] 
		{i in 1..7} <tempCalendDOW[rota,data_inicio,data_final,i] = col(substr(DOW[i],1,3))>;
	for{<z,di,df> in CalendSet, d in 1..7}
		if tempCalendDOW[z,di,df,d] ~= '' then calendDOW[z,di,df,d] = 1;

/* Calcula total de CNs por setor*/
	num totalCN{DataAlvoSet,CDSetorSet};
	num prazoMax{DataAlvoSet,CDSetorSet};
	num prazo2Max{DataAlvoSet,CDSetorSet};
	num dataCompr{DataAlvoSet,CDSetorSet};
	num dataCompr2{DataAlvoSet,CDSetorSet};
	num percPrazo2Max{DataAlvoSet,CDSetorSet};
	num prazo{RotaZNSet};
	num p, dc, exit;
	set diasCal;
	set rotaCal; 
	for{da in DataAlvoSet} do;
		rotaCal = setof{<rt,di,df> in CalendSet: di<=da<=df}<rt>;
		for{<cd,st,dis,dfs> in CDSetorSet: dis<=da<=dfs } do;
			totalCN[da,cd,st,dis,dfs] = sum{<(st),zn,die,dfe> in EComSet: die<=da<=dfe} QTD_CN[st,zn,die,dfe];
			/* Calcula Prazo Máximo*/
			prazoMax[da,cd,st,dis,dfs] = 0;
			dataCompr2[da,cd,st,dis,dfs] = da;
			dataCompr[da,cd,st,dis,dfs] = da;
			for{<(st),zn,die,dfe> in EComSet, <rt,(zn),dir,dfr> in RotaZNSet: dir<=da<=dfr and die<=da<=dfe} do;
				/* Soma o prazo do CD = 1 dia*/
				prazo[rt,zn,dir,dfr] = prazo_contratado[rt,zn,dir,dfr];
				p =  prazo[rt,zn,dir,dfr] + 1;
				dc = da;
				do while(p>0);
					dc = dc - 1;
					if weekday(dc) not in {1,7} and <(cd),dc> not in FeriadoSet then 
						p = p - 1;
					if p=0 and rt in rotaCal then do;
						diasCal = setof{d in 1..7, <(rt),dic,dfc> in CalendSet: dic<=da<=dfc and calendDOW[rt,dic,dfc,d]=1}<d>;
						exit = 0;
						do while(!exit);
							if weekday(dc) in diasCal and <(cd),dc> not in FeriadoSet then exit=1;
							else do;
								dc = dc - 1;
								if weekday(dc) not in {1,7} and <(cd),dc> not in FeriadoSet then 
									prazo[rt,zn,dir,dfr] = prazo[rt,zn,dir,dfr] + 1;
							end;
						end;
					end;
				end;
				if prazo[rt,zn,dir,dfr] > prazoMax[da,cd,st,dis,dfs] then do;
					prazoMax[da,cd,st,dis,dfs] = prazo[rt,zn,dir,dfr];
					dataCompr2[da,cd,st,dis,dfs] = dataCompr[da,cd,st,dis,dfs];
					dataCompr[da,cd,st,dis,dfs] = dc;
				end;
			end;
			prazo2Max[da,cd,st,dis,dfs] = max{<(st),zn,die,dfe> in EComSet, <rt,(zn),dir,dfr> in RotaZNSet: 
				dir<=da<=dfr and die<=da<=dfe and prazo[rt,zn,dir,dfr]~=prazoMax[da,cd,st,dis,dfs]} prazo[rt,zn,dir,dfr];
			percPrazo2Max[da,cd,st,dis,dfs] = sum{<(st),zn,die,dfe> in EComSet, <rt,(zn),dir,dfr> in RotaZNSet: 
				dir<=da<=dfr and die<=da<=dfe and prazo[rt,zn,dir,dfr]~=prazoMax[da,cd,st,dis,dfs]}
					QTD_CN[st,zn,die,dfe] / totalCN[da,cd,st,dis,dfs];
			if prazo2Max[da,cd,st,dis,dfs] < 1 then prazo2Max[da,cd,st,dis,dfs] = 0;
		end;
	end;
	create data DATA_COMPROMISSO from [DATA_ALVO COD_CD COD_SETOR DATA_INI DATA_FIN]=
		{da in DataAlvoSet,<cd,st,dis,dfs> in CDSetorSet: dis<=da<=dfs}
		prazoMax prazo2Max percPrazo2Max dataCompr dataCompr2;
quit;

PROC SQL;
   CREATE TABLE SAIDA_DATA_COMPROMISSO AS 
   SELECT t2.UR, 
          t2.COD_RE, 
          t2.RE, 
          t2.COD_GV, 
          t2.GV, 
          t2.COD_SETOR, 
          t2.SETOR, 
          t3.ROTA, 
          t2.ZONEAMENTO, 
          t3.nome_cidade, 
          t1.COD_CD, 
          1 AS PRAZO_CD, 
          t1.prazoMax AS PRAZO_CONTRATADO, 
          t1.prazo2Max AS PRAZO_CONTRATADO2, 
          t1.percPrazo2Max FORMAT=percent8.0 AS PERC_PRAZO2, 
          t1.dataCompr FORMAT=date7. AS DATA_COMPROMISSO, 
          t1.dataCompr2 FORMAT=date7. AS DATA_COMPROMISSO2, 
          t1.DATA_ALVO FORMAT=date7.
      FROM WORK.DATA_COMPROMISSO t1, SIMULA.ESTRUTURA_COMERCIAL t2, SIMULA.ROTA_ZONEAMENTO t3
      WHERE (t1.COD_SETOR = t2.COD_SETOR AND t2.ZONEAMENTO = t3.ZONEAMENTO);
QUIT;
