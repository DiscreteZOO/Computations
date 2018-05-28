# test whether abelian invariants can be mapped
BindGlobal("CanMapFiniteAbelianInvariants",function(from,to)
local pf,pt,fp,tp,p,i,f;
  # first get primes and then run for each prime
  pf:=Union(List(from,Factors));
  pt:=Union(List(to,Factors));
  if not IsSubset(pf,pt) then
    return false;
  fi;
  for p in pf do
    fp:=[];
    for i in from do
      f:=Filtered(Factors(i),x->x=p);
      if Length(f)>0 then
        Add(fp,Product(f));
      fi;
    od;
    tp:=[];
    for i in to do
      f:=Filtered(Factors(i),x->x=p);
      if Length(f)>0 then
        Add(tp,Product(f));
      fi;
    od;
    #Print(fp,tp,"\n");
    if Length(fp)<Length(tp) then return false;fi;
    Sort(fp);Sort(tp);
    fp:=Reversed(fp);
    tp:=Reversed(tp);
    if ForAny([1..Length(tp)],i->fp[i]<tp[i]) then
      return false;
    fi;
  od;
  return true;
end);


#############################################################################
##
#F  GQuotients(<F>,<G>)  . . . . . epimorphisms from F onto G up to conjugacy
##
InstallMethod(GQuotients,"whole fp group to finite group",true,
  [IsSubgroupFpGroup and IsWholeFamily,IsGroup and IsFinite],1,
function (F,G)
local Fgens,	# generators of F
      rels,	# power relations
      cl,	# classes of G
      imgo,imgos,sel,
      e,	# excluded orders (for which the presentation collapses
      u,	# trial generating set's group
      pimgs,	# possible images
      val,	# its value
      i,j,	# loop
      ma,
      dp,emb1,emb2, # direct product
      sameKernel,
      A,bigG,Gmap,opt,
      h;	# epis

  Fgens:=GeneratorsOfGroup(F);

  if Length(Fgens)=0 then
    if Size(G)>1 then
      return [];
    else
      return [GroupHomomorphismByImagesNC(F,G,[],[])];
    fi;
  fi;

  if Size(G)=1 then
    return [GroupHomomorphismByImagesNC(F,G,Fgens,
			  List(Fgens,i->One(G)))];
  elif Length(Fgens)=1 then
    Info(InfoMorph,1,"Cyclic group: only one quotient possible");
    # a cyclic group has at most one quotient

    # force size (in abelian invariants)
    e:=AbelianInvariants(F);

    if not IsCyclic(G) or (IsFinite(F) and not IsInt(Size(F)/Size(G))) then
      return [];
    else
      # get the cyclic gens
      h:=First(AsList(G),i->Order(i)=Size(G));
      # just map them
      return [GroupHomomorphismByImagesNC(F,G,Fgens,[h])];
    fi;
  fi;

  # try abelian part first
  if not IsPerfectGroup(G) then
    ma:=ShallowCopy(AbelianInvariants(F));
    for i in [1..Length(ma)] do
      if ma[i]=0 then ma[i]:=Size(G);fi; # the largest interesting bit
    od;
    if CanMapFiniteAbelianInvariants(ma,AbelianInvariants(G))=false then
      return [];
    fi;
  fi;

  bigG:=G; # generic settings
  Gmap:=fail;

  # try to reduce with automorphisms
  if IsSolvableGroup(G) and Length(Fgens)>2 
      and ValueOption("noauto")<>true then
    A:=AutomorphismGroup(G);
    if (IsSolvableGroup(A) or Size(G)<10000) and 
        not ForAll(GeneratorsOfGroup(A),IsInnerAutomorphism) then

      # could decide based on HasGeneralizedPcgs...SemidirectProduct(A,G);
      i:=IsomorphismPermGroup(A); # IsomorphismPc might be composition
      bigG:=SemidirectProduct(Image(i),InverseGeneralMapping(i),G);
      Gmap:=Embedding(bigG,2);
      G:=Image(Gmap);
      Gmap:=InverseGeneralMapping(Gmap);
    fi;
  fi;

  cl:=Filtered(ConjugacyClasses(bigG),x->Representative(x) in G);

  # search relators in only one generator
  rels:=ListWithIdenticalEntries(Length(Fgens),false);

  for i in RelatorsOfFpGroup(F) do
    if NrSyllables(i)=1 then
      # found relator in only one generator
      val:=Position(List(FreeGeneratorsOfFpGroup(F),j->GeneratorSyllable(j,1)),
                    GeneratorSyllable(i,1));
      u:=AbsInt(ExponentSyllable(i,1));
      if rels[val]=false then
	rels[val]:=u;
      else
	rels[val]:=Gcd(rels[val],u);
      fi;
    fi;
  od;


  # exclude orders
  e:=Set(List(cl,i->Order(Representative(i))));
  e:=List(Fgens,i->ShallowCopy(e));
  for i in [1..Length(Fgens)] do
    if rels[i]<>false then
      e[i]:=Filtered(e[i],j->rels[i]<>j and IsInt(rels[i]/j));
    fi;
  od;
  e:=ExcludedOrders(F,e);

  # find potential images
  pimgs:=[];

  for i in [1..Length(Fgens)] do
    if rels[i]<>false then
      Info(InfoMorph,2,"generator order must divide ",rels[i]);
      u:=Filtered(cl,j->IsInt(rels[i]/Order(Representative(j))));
    else
      Info(InfoMorph,2,"no restriction on generator order");
      u:=ShallowCopy(cl);
    fi;
    u:=Filtered(u,j->not Order(Representative(j)) in e[i]);
    Add(pimgs,u);
  od;

  val:=Product(pimgs,i->Sum(i,Size));
  Info(InfoMorph,1,List(pimgs,Length)," possibilities, Value: ",val);

  val:=1;
  opt:=rec(gens:=Fgens,to:=bigG,
        from:=F, free:=FreeGeneratorsOfFpGroup(F),
        rels:=List(RelatorsOfFpGroup(F),i->[i,1]));

  if G=bigG then
    val:=val+4; # surjective
  else
    opt.condition:=hom->Size(Image(hom))=Size(G);
  fi;

  if ValueOption("findall")<>false then
    val:=val+8; # onlyone
  fi;
  h:=MorClassLoop(bigG,pimgs,opt,val);
  if not IsList(h) then h:=[h];fi;

  #if ForAny(h,x->opt.condition(x)=false) then Error("CRAP");fi;

  Info(InfoMorph,1,"Found ",Length(h)," maps, test kernels");

  dp:=DirectProduct(G,G);
  emb1:=Embedding(dp,1);
  emb2:=Embedding(dp,2);
  sameKernel:=function(m1,m2)
  local a;
    m1:=MappingGeneratorsImages(m1)[2];
    m2:=MappingGeneratorsImages(m2)[2];
    a:=List([1..Length(Fgens)],i->
      ImagesRepresentative(emb1,m1[i])*ImagesRepresentative(emb2,m2[i]));
    return Size(SubgroupNC(dp,a))=Size(G);
  end;

  imgos:=[];
  cl:=[];
  u:=[];
  for i in h do
    imgo:=List(Fgens,j->Image(i,j));
    imgo:=Concatenation(imgo,MorFroWords(imgo));
    # fingerprint: Order of fros and commuting indication
    imgo:=Concatenation(List(imgo,Order),
      Concatenation(List([1..Length(imgo)],
        a->Filtered([a+1..Length(imgo)],x->IsOne(Comm(imgo[a],imgo[x]))))));
    sel:=Filtered([1..Length(imgos)],i->imgos[i]=imgo);
    #Info(InfoMorph,3,"|sel|=",Length(sel));
    if Length(sel)=0 then
      Add(imgos,imgo);
      Add(cl,i);
    else
      for j in sel do
	if not IsBound(u[j]) then
	  u[j]:=KernelOfMultiplicativeGeneralMapping(cl[j]);
	fi;
      od;
      
      #e:=KernelOfMultiplicativeGeneralMapping(i);
      if not ForAny(cl{sel},x->sameKernel(x,i)) then
	Add(imgos,imgo);
	Add(cl,i);
	#u[Length(cl)]:=e;
      fi;

    fi;
  od;

  Info(InfoMorph,1,Length(h)," found -> ",Length(cl)," homs");
  if Gmap<>fail then
    cl:=List(cl,x->x*Gmap);
  fi;
  return cl;
end);

