open Prelude

module SaferStdLib = struct
  let slice starting ending =
    let open String in
    drop starting << take ending

  let string_to_int str =
    Result.trapc
      "tried to convert non-digit characters to int"
      int_of_string
      str
    
  let assoc_res key alist = match assoc_opt key alist with
    | Some x -> Ok x
    | None -> Error "key error"
end

module type TABLE = sig
  type t
  type field
  type subfield
  type error
  val lookup : field -> subfield -> t -> (string, error) result
end

module Table = struct
  type t = string
  type field = string
  type subfield = char
  type error = string

  include SaferStdLib
                    
  module FakeSeek = struct

    module E = struct type t = string end
    module R = Endofunctors.Result.Make (E)
    include R
       
    let leader table = slice 0 24 table

    let base_pos table = slice 12 17 table
                         |> string_to_int
    
    let raw_directory table =
      let starting = 24 in
      let* ending = base_pos table in
      pure (slice starting ending table)
    
    let directory table =
      let rec directory' dir =
        let len = String.length dir in
        if len >= 12
        then let field = slice 0 3 dir in
             let value_length = slice 3 7 dir in
             let value_pos = slice 7 12 dir in
             let remainder = slice 12 len dir in
             (field, (value_length, value_pos)) :: directory' remainder
        else []
      in
      let+ raw = raw_directory table
      in directory' raw

    let lookup field _ marc =
      let* bpos = base_pos marc in
      let* dir = directory marc in
      let* (l_str, pos_str) = assoc_res field dir in
      let* l = string_to_int l_str in
      let* p = string_to_int pos_str in
      let body = String.drop bpos marc in
      pure (slice p (p + l) body)
  end

  let lookup = FakeSeek.lookup

  module Examples = struct
    let a = "02519nam a2200469 i 450000100070000000500170000700800410002400300040006501000160006902000100008503500200009503500190011504000180013404100080015205000330016008200250019310001120021824500950033026001380042530000530056333600730061633700710068933800690076044000330082950001790086250400290104165000740107065000620114465000580120665000590126465500650132365500640138870001310145270001030158370000580168690100130174490300090175792900080176699900790177492800840185392701120193726931220071211144300.0790409s1976    dcua     b   f00010 eng uICU  a   76600071  c$3.25  a(ICU)BID3921390  a(OCoLC)2935667  aDLCcDLCdICU0 aeng0 aQC100b.U57 no. 440aQC494.3  a602/.1 sa535.6/01/41 aKelly, Kenneth L.,d1910-19910http://id.loc.gov/authorities/names/n500481531http://viaf.org/viaf/9194448210aColor :buniversal language and dictionary of names /cKenneth L. Kelly and Deane B. Judd.0 a[Washington] :bU.S. Dept. of Commerce, National Bureau of Standards : for sale by the Supt. of Docs., U.S. Govt. Print. Off.,c1976.  avii, 19, v, 158 p. :bill. (some col.) ;c26 cm.  atextbtxt2rdacontent0http://id.loc.gov/vocabulary/contentTypes/txt  aunmediatedbn2rdamedia0http://id.loc.gov/vocabulary/mediaTypes/n  avolumebnc2rdacarrier0http://id.loc.gov/vocabulary/carriers/nc 0aNBS special publicationv440  aSupersedes and combines The ISCC-NBS method of designating colors and a dictionary of color names by K. L. Kelly and D. B. Judd and A universal color language by K. L. Kelly.  aIncludes bibliographies. 0aColorxTerminology0http://id.loc.gov/authorities/subjects/sh85028581 0aColors0http://id.loc.gov/authorities/subjects/sh85028700 7aColor.2fast0http://id.worldcat.org/fast/fst00868499 7aColors.2fast0http://id.worldcat.org/fast/fst00868751 7aDictionaries.2fast0http://id.worldcat.org/fast/fst01423826 7aTerminology.2fast0http://id.worldcat.org/fast/fst0142388010aJudd, Deane Brewster,d1900-1972.ejoint author.0http://id.loc.gov/authorities/names/n500388371http://viaf.org/viaf/5293114811aKelly, Kenneth Low,d1910-tISCC-NBS method of designating colors and a dictionary of color names.11aKelly, Kenneth Low,d1910-tUniversal color language.  aAnalytic  aHeVa  acatffiba379fcc-28a9-5e0b-bf69-aa4b73df837bs475ef984-4515-5f26-b238-1d98c61d2b70  tLibrary of Congress classificationaQC100.U524 no.440lASRcASR-SciASRi1712591  tLibrary of Congress classificationaQC100.U524 no.440lASRcASR-SciASRgAnalyticeCRERARbA13099231i871191"
    let b = "03353ntm a22004213i 45000010009000000050017000090070015000260080041000410030004000820200018000860350024001040350015001280400031001431000032001742450082002062600010002882640058002983000038003563360026003943370026004203380036004465000098004825020098005805060059006785100074007375201457008115460012022685900022022806900016023026900017023187100057023357200038023927200044024308560149024749290014026239990079026379280215027161077319020151014160112.5cr un|---|||||151014s2015    miu||||||m   |||||||eng dICU  a9781321918724  a(MiAaPQD)AAI3714522  aAAI3714522  aMiAaPQDbengcMiAaPQDerda1 aTeichman, Matthew,eauthor.10aCharacterizing kinds: A semantics for generic sentences /cTeichman, Matthew.  c2015. 1aAnn Arbor : bProQuest Dissertations & Theses, c2015  a1 electronic resource (230 pages)  atextbtxt2rdacontent  acomputerbc2rdamedia  aonline resourcebcr2rdacarrier  aAdvisors: Jason Bridges; Christopher Kennedy  Committee members: Frank Veltman; Malte Willer.  bPh.D.cThe University of Chicago, Division of the Humanities, Department of Philosophyd2015.  aThis item must not be sold to any third party vendors.4 aDissertation Abstracts International, cVolume: 76-12(E), Section: A.  aIn this text, I argue that generic statements---statements of the form Fs are G, such as 'Bears are furry'---are particular statements about kinds, rather than general statements about individual objects. Although statements of this form intuitively seem like generalizations, I claim that in this case, appearances are deceptive. First, I present new linguistic evidence which raises problems for the standard quantificational theory of generic sentences, according to which generic sentences contain a hidden, unpronounced quantifier. Though the simple kind theory has served as a standard alternative to quantificational approaches in the literature on generics since Carlson (1977), it also has a more sophisticated cousin, which has largely been ignored. I develop an extension of the sophisticated kind theory and show how it can neatly account for these phenomena while sidestepping the standard objections to the simple kind theory. At a broader level, I would like to claim that if a kind theory provides the best explanation for the truth conditions of these sentences in English, then it tells us something interesting about English speakers: namely, that in virtue of their speaking English, they implicitly presuppose an ontology with kinds as possible objects. In this way, I suggest, the search for the best semantic theory of generic sentences has the potential to lead us towards a new, philosophically valuable conception of kindhood.  aEnglish  aSchool code: 0330  aPhilosophy.  aLinguistics.2 aUniversity of Chicago.edegree granting institution.1 aJason Bridgesedegree supervisor.1 aChristopher Kennedyedegree supervisor.40uhttp://gateway.proquest.com/openurl?url_ver=Z39.88-2004&rft_val_fmt=info:ofi/fmt:kev:mtx:dissertation&res_dat=xri:pqm&rft_dat=xri:pqdiss:3714522  aeresourceffi8378e8eb-b489-5dba-aca6-a588f3d81d0as5887d0b7-ccac-59e6-8c51-7a05d621dd86  tLibrary of Congress classificationlOnlinecUC-FullTextuhttp://gateway.proquest.com/openurl?url_ver=Z39.88-2004&rft_val_fmt=info:ofi/fmt:kev:mtx:dissertation&res_dat=xri:pqm&rft_dat=xri:pqdiss:3714522i9079414"
    let c = "07824cgm a2201117Ia 45000010008000000030004000080050017000120070010000290070010000390080041000490200018000900200015001080200018001230200015001410280055001560280060002110280063002710280057003340350021003910400023004120410028004350430021004630490009004840900024004932450162005172460016006792500061006952600053007563000059008093360097008683370066009653370066010313380072010973380072011694900041012415000225012825000026015075000066015335000060015995000281016595380117019405380081020575460059021385460055021975110076022525110192023285080080025205080248026005200246028485200289030946500052033836500037034356500062034726500079035346500060036136500076036736510067037496510060038166510067038766550058039436550066040016550066040677000134041337000116042677000112043837000042044957000113045377000106046507000106047567000118048627000027049807000040050077000102050477000115051497000105052647000067053697000032054367000030054687000121054987000101056197000106057207000105058267100048059317100026059797100112060057300107061178300050062248300050062748300049063248300049063739030009064229290008064319990079064399280087065189270101066059797309ICU20131220105400.0vd msaizmvd mvaizm131213p20131936xxu148            vlwol d  a9781604658132  a1604658134  a9781604658149  a160465814242aCC2333BDDVDbThe Criterion Collectionq(container)42aCC2333BDDVD-1bThe Criterion Collectionq(Blu-ray disc)42aCC2333BDDVD-2bThe Criterion Collectionq(Touki Bouki DVD)42aCC2333BDDVD-3bThe Criterion Collectionq(Redes DVD)  a(OCoLC)865173585  aJBLcJBLdJBLdCGU0 awolaspajenghwolhspa  af-sg---anm-----  aCGUA  aPN1997b.M3763 201300aMartin Scorsese's World Cinema Project.nNo. 1,pTouki bouki /ca film by Djibril Diop Mambéty. Redes / a film by Fred Zinnemann and Emilio Gómez Muriel.30aTouki bouki  aDual-format ed., Dual-format Blu-ray and DVD special ed.  a[United States] :bCriterion Collection,cc2013.  a3 videodiscs (148 min.) :bsd., col., b&w ;c4 3/4 in.  atwo-dimensional moving imagebtdi2rdacontent0http://id.loc.gov/vocabulary/contentTypes/tdi  avideobv2rdamedia0http://id.loc.gov/vocabulary/mediaTypes/v  avideobv2rdamedia0http://id.loc.gov/vocabulary/mediaTypes/v  avideodiscbvd2rdacarrier0http://id.loc.gov/vocabulary/carriers/vd  avideodiscbvd2rdacarrier0http://id.loc.gov/vocabulary/carriers/vd1 aThe Criterion collection ;v685, 686  aThe single Blu-ray disc and the set of two DVDs each contains the World Cinema Project's 2K digitally-restored versions of the feature films Touki bouki (89 min.) and Redes (59 min.) and a collection of special features.  aTitle from container.  aTouki bouki: Originally produced as a motion picture in 1973.  aRedes: Originally produced as a motion picture in 1936.  aSpecial features on Blu-ray disc and the two DVDs include new introductions to the films by World Cinema Project founder Martin Scorsese, a new interview with filmmaker Abderrahmane Sissako about Touki bouki, and a new visual essay on Redes by filmmaker and writer Kent Jones.  aBlu-ray, region A; full screen (1.37:1 or 1.33:1) presentations; uncompressed monaural; requires Blu-ray player.  aDVD, NTSC, region 1; full screen (1.37:1 or 1.33:1) presentations; monaural.  aTouki bouki: In Wolof with optional English subtitles.  aRedes: In Spanish with optional English subtitles.1 aTouki bouki: Magaye Niang, Mareme Niang, Aminata Fall, Ousseuynou Diop.1 aRedes: Silvio Hernández del Valle, Antonio Lara, Miguel Figueroa, Rafael Hinojosa, Felipe Rojas, David Valle González, Susana Ortiz Cobos, and the fishermen and neighbors of Alvarado.  aTouki bouki: Director of photography, George Bracher ; editor, Siro Asteni.  aRedes: Producer, Secretaría de Educatión Pública, Mexico City ; written by Augustín Velásquez Chávez, Paul Strand ; cinematographer, Paul Strand ; editors, Gunther von Fritsch, Emilio Gómez Muriel ; composer, Silvestre Revueltas.  aTouki bouki: \"A vivid, fractured portrait of Senegal in the early 1970s. ... two young lovers long to leave Dakar for the glamour and comforts of France, but their escape plan is beset by complications both concrete and mystical\"--Container.  aRedes: \"This vivid, documentary-like dramatization of the daily grind of men struggling to make a living by fishing on the Gulf of Mexico (mostly played by real-life fishermen), one worker's terrible loss instigates a political awakening among him and his fellow laborers\"--Container. 0aMan-woman relationshipszSenegalzDakarvDrama. 0aFisherszMexico, Gulf ofvDrama. 7aCriminals.2fast0http://id.worldcat.org/fast/fst00883516 7aEmigration and immigration.2fast0http://id.worldcat.org/fast/fst00908690 7aFishers.2fast0http://id.worldcat.org/fast/fst00926174 7aMan-woman relationships.2fast0http://id.worldcat.org/fast/fst01007080 7aGulf of Mexico.2fast0http://id.worldcat.org/fast/fst01239980 7aSenegal.2fast0http://id.worldcat.org/fast/fst01204328 7aSenegalzDakar.2fast0http://id.worldcat.org/fast/fst01206416 7aDrama.2fast0http://id.worldcat.org/fast/fst01423879 7aFeature films.2fast0http://id.worldcat.org/fast/fst01710384 7aFiction films.2fast0http://id.worldcat.org/fast/fst017102641 aDiop Mambéty, Djibril,d1945-1998.4drt4pro4aus0http://id.loc.gov/authorities/names/nr960245261http://viaf.org/viaf/97445671 aZinnemann, Fred,d1907-1997.4drt0http://id.loc.gov/authorities/names/n882448991http://viaf.org/viaf/100381771 aGómez Muriel, Emilio.4drt0http://id.loc.gov/authorities/names/nr970388751http://viaf.org/viaf/956674021 aVelásquez Chávez, Agustín.4aus1 aStrand, Paul,d1890-1976.4aus0http://id.loc.gov/authorities/names/n801538111http://viaf.org/viaf/887142601 aNiang, Magaye.4act0http://id.loc.gov/authorities/names/no20030254411http://viaf.org/viaf/2901964371 aNiang, Mareme.4act0http://id.loc.gov/authorities/names/no20080790881http://viaf.org/viaf/3114315331 aFall, Aminata,d1942-2002.4act0http://id.loc.gov/authorities/names/no20081771051http://viaf.org/viaf/2330999641 aDiop, Ousseuynou.4act1 aDel Valle, Silvio Hernández.4act1 aLara, Antonio.4act0http://id.loc.gov/authorities/names/n820847791http://viaf.org/viaf/617833601 aFigueroa Toro, Miguel E.4act0http://id.loc.gov/authorities/names/no20060598981http://viaf.org/viaf/465166921 aHinojosa, Rafael.4act0http://id.loc.gov/authorities/names/n878540241http://viaf.org/viaf/285961171 aRojas, Felipe.4act1http://viaf.org/viaf/111499123155062101091 aGonzalez, David Valle.4act1 aCobos, Susana Ortiz.4act1 aRevueltas, Silvestre,d1899-1940.4cmp0http://id.loc.gov/authorities/names/n831271951http://viaf.org/viaf/346443631 aScorsese, Martin.0http://id.loc.gov/authorities/names/n810503791http://viaf.org/viaf/1117161451 aSissako, Abderrahmane.0http://id.loc.gov/authorities/names/no990636411http://viaf.org/viaf/377819381 aJones, Kent,d1964-0http://id.loc.gov/authorities/names/no20080387411http://viaf.org/viaf/416442412 aSecretaría de Educatión Pública.4pro2 aWorld Cinema Project.2 aCriterion Collection (Firm)0http://id.loc.gov/authorities/names/no980926221http://viaf.org/viaf/15859010502aRedes (Motion picture)0http://id.loc.gov/authorities/names/nr000346021http://viaf.org/viaf/178134612 0aCriterion collection (DVD videodiscs) ;v685. 0aCriterion collection (DVD videodiscs) ;v686. 0aCriterion collection (Blu-ray discs) ;v685. 0aCriterion collection (Blu-ray discs) ;v686.  aHeVa  acatffi0ed38c15-1b2c-50f1-8c92-792fb87e5b60s6013ac95-362c-5767-acb2-8e88436d320f  tLibrary of Congress classificationaPN1997.M3763 2013pDVDlJRLcJRL-Filmi1145891  tLibrary of Congress classificationaPN1997.M3763 2013pDVDw3lJRLcJRL-Filmb109357536i9236455"
  end
end

module _ : TABLE = Table
