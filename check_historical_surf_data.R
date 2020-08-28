library(data.table)
library(dplyr)
library(stringdist)
file.name = "~/Downloads/KL_surf_ws_surf_ED2_ED0_CN_surf.csv"

check.col.name = "method"
method = "cosine"
dist_t = 0.05 
main_parties = c("INC","BJP")

surf_ws = fread(file.name,na="")
surf_ws = surf_ws[order(surf_ws$Year,-surf_ws$Election_Type,surf_ws$Assembly_No, surf_ws$Constituency_No,surf_ws$Poll_No,surf_ws$Position),]
merge_pids = function(data, pids){
  idxs = which(data$ID %in% pids)
  c_data = data[idxs,]
  c_data =c_data[order(c_data$Year,-c_data$Election_Type,c_data$Poll_No),]
  pid_to_get = c_data$ID[1]
  print(paste("updated pid :", pid_to_get))
  data$ID[idxs] = pid_to_get
  
  return(data.table(data))
}

if(!check.col.name %in% names(surf_ws)){
  surf_ws[[check.col.name]] = NA
}
checked.rows = which(surf_ws[[check.col.name]] == method)
if(length(checked.rows)>0){
  last.checked.row = max(checked.rows)
}else{
  last.checked.row =1
}

# ae = surf_ws[Election_Type=="AE"]
# min_assembly = min(ae$Assembly_No)
# max_assembly = max(ae$Assembly_No)
min_year = min(surf_ws$Year[last.checked.row:nrow(surf_ws)])
max_year = max(surf_ws$Year[last.checked.row:nrow(surf_ws)])




iter_year = 0
suggestion = 0
matched= 0
unmatch=0

for(i in min_year:(max_year-1)){
  iter_year = iter_year+1
  
  year_cands = which(surf_ws$Year == i & (surf_ws$Position %in% c(1,2) | surf_ws$Party %in% main_parties) & (as.integer(row.names(surf_ws)) >= last.checked.row))
  if(i+30 <= max_year){
    gen_yr= i+30
  }else{
    gen_yr= max_yr
  }
  next_gen_cands = which(surf_ws$Year %in% (i+1):(gen_yr) & (as.integer(row.names(surf_ws)) >= last.checked.row))
  #unmerged_pids = which(!surf_ws$ID[year_cands] %in% surf_ws$ID[next_gen_cands])
  curr_pids = unique(surf_ws$ID[year_cands])
  next_gen_pids = unique(surf_ws$ID[next_gen_cands])
  unmerged.pids = curr_pids[which(!curr_pids %in% next_gen_pids)]
  unmerged.cands = which(surf_ws[year_cands]$ID %in% unmerged.pids)

  for(p in unmerged.cands){
    cand_name = surf_ws$Candidate[year_cands[p]]
    row.number = year_cands[p]
    pids_to_merge = NULL
    print(paste("matching candidate",cand_name, "from party:", surf_ws$Party[year_cands[p]],", Election_Type :", surf_ws$Election_Type[year_cands[p]],", Constituency Name :", surf_ws$Constituency_Name[year_cands[p]],", Position :",surf_ws$Position[year_cands[p]], ", Year:",surf_ws$Year[year_cands[p]], ", ID:",surf_ws$ID[year_cands[p]]))
    idx = stringdist(cand_name,surf_ws$Candidate[next_gen_cands],method = method) #merging on basis of sound
    
    x = which(idx <= dist_t) # value 0 is when there is no difference in sound
    if(length(x) != 0){
      suggestion = suggestion+1
      
      matching_cases = surf_ws[next_gen_cands[x]]
      matching_cases$dist = stringdist(cand_name,matching_cases$Candidate,method="jw",p=0.1)
      matching_cases = matching_cases[order(matching_cases$dist,matching_cases$Year),]
      
      same_const_party = which(matching_cases$Party == surf_ws$Party[year_cands[p]] & tolower(matching_cases$Constituency_Name) == tolower(surf_ws$Constituency_Name[year_cands[p]]))
      if(length(same_const_party) >0){
        print("Same party and constituency candidates found")
        print(matching_cases[same_const_party,c("State_Name","Election_Type","Year","Candidate","Party","Constituency_Name","Position","ID")])
        to_select = readline(prompt = "select row numbers which matches the candidate (space seprated list):")
        sel = as.integer(strsplit(to_select," ")[[1]]) #pick number of the correct village to map
        if(length(sel) >0){
          
          print("matched candidates")
          print(matching_cases[same_const_party[sel],c("Election_Type","Year","Candidate","Party","Constituency_Name","Position","ID")])
          pids_to_merge = c(pids_to_merge,unique(matching_cases$ID[same_const_party[sel]]))
        }
        
        pids_to_merge = c(pids_to_merge,unique(matching_cases$ID[same_const_party]))
      }else{
        print("no similar candidate found in same party and Constituency")
      }
      
      same_const_or_party = which(matching_cases$Party == surf_ws$Party[year_cands[p]] | tolower(matching_cases$Constituency_Name) == tolower(surf_ws$Constituency_Name[year_cands[p]])) %>% setdiff(same_const_party)
      if(length(same_const_or_party) >0){
        print("other matching candidates found")
        print(matching_cases[same_const_or_party,c("Election_Type","Year","Candidate","Party","Constituency_Name","Position","ID")])
        
        to_select = readline(prompt = "select row numbers which matches the candidate (space seprated list):")
        sel = as.integer(strsplit(to_select," ")[[1]]) #pick number of the correct village to map
        if(length(sel) >0){
          
          print("matched candidates")
          print(matching_cases[same_const_or_party[sel],c("Election_Type","Candidate","Party","Constituency_Name","Position","ID")])
          pids_to_merge = c(pids_to_merge,matching_cases$ID[same_const_or_party[sel]])
        }
        
      }else{
        print("no similar candidate found in same party or Constituency")
      }
      if(length(pids_to_merge)>0){
        matched=matched+length(pids_to_merge)
        pids_to_merge = c(pids_to_merge,surf_ws$ID[year_cands[p]])
        surf_ws[[check.col.name]][year_cands[p]] = method
        print(paste("to merge pids :", paste(pids_to_merge,collapse = ",")))
        surf_ws= merge_pids(surf_ws,pids_to_merge)
      }else{
        unmatch = unmatch+1
      }
      
    }else{
      unmatch = unmatch+1
    }
    
    
  }
  
}
  
fwrite(surf_ws,file.name,na="")
