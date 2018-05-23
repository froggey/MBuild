(in-package :eliza)

(defparameter *viewpoint* '((I . you) (you . me) (me . you) (am . are) (yourself . myself) (your . my) (my . your)))

(defparameter *eliza-rules*
 '((((?* ?x) hello (?* ?y))      
    (How do you do.  I am a bot.))

   (((?* ?x) fuck (?* ?y))
    (|That's| not very polite))

   (((?* ?x) shit (?* ?y))
    (|That's| not very polite))

   (((?* ?x) ass (?* ?y))
    (|Can't| you be a bit more polite?))

   (((?* ?x) me harder)
    ("MORE" ?x))

   ((more (?* ?x))
    (?x me harder))

   ((what s the difference between (?* ?x) and (?* ?y))
    (?x has similar features but longer hair)
    (?y has similar features but longer hair))

   ((what are the differences between (?* ?x) and (?* ?y))
    (?x has similar features but longer hair)
    (?y has similar features but longer hair))

   ((what is the difference between (?* ?x) and (?* ?y))
    (?x has similar features but longer hair)
    (?y has similar features but longer hair))

   ((the difference between (?* ?x) and (?* ?y))
    (?x has similar features but longer hair)
    (?y has similar features but longer hair))

   ((the differences between (?* ?x) and (?* ?y))
    (?x has similar features but longer hair)
    (?y has similar features but longer hair))

   (((?* ?x) you (?* ?y) written (?* ?z))
    (|I'm| written in Common Lisp))

   (((?* ?x) bot (?* ?y))
    (|I'm| not a |bot.| I prefer the term |``electronically composed''.|))

   #-armedbear
   ((seen ?x)
    (?x was last seen 5y6m14d32h43m10s |ago,| saying |"minion: when are you going to support seen?"|))

   (((?* ?x) did you (?* ?y))
    (|no, I didn't| ?y)
    (|yes, I| ?y))

   ((are you (?* ?x))
    (yes)
    (no)
    (maybe))

   ((botsnack)
    (Thanks!))

   ((bot snack)
    (Thanks!))

   ((welcome (?* ?y))
    (Thanks!))

   ((not much) (good))

   #-armedbear
   (((?* ?x) linux (?* ?y))
    (I run on Crux Linux - |http://www.crux.nu/|))

   #-armedbear
   (((?* ?x) crux (?* ?y))
    (I like running on Crux Linux))

   (((?* ?x) slackware (?* ?y))
    (Slackware is nice but I like Crux))
   #-armedbear
   (((?* ?x) debian (?* ?y))
    (|baah, use crux: http://www.crux.nu/|))

   ((how are you (?* ?x))
    (|It's| going quite fine today))

   (((?* ?x) it going)
    (pretty good))

   (((?* ?x) common lisp (?* ?y))
    (Common Lisp is good)
    (|I'm| written in Common Lisp))

   (((?* ?x) scheme (?* ?y))
    (Scheme is the root of all evil))

   ((what do you think (?* ?x) bot)
    (if |it's| not written in |lisp,| |it's| not any good))

   ((go (?* ?x))
    (you first)
    (after you)
    (have you tried to ?x yourself?))

   ((have you (?* ?x))
    (I have ?x)
    (I have)
    (I |haven't| ?x)
    (I |haven't|)
    (ISTR having ?x IIRC))

   (((?* ?y) your mother (?* ?x))
    (and your father smelt of elderberries))

   (((?* ?x) your father (?* ?y))
    (your mother was a hamster))

   (((?* ?x) seen you (?* ?y))
    (I |haven't| seen you ?y either))

   ((you are (?* ?x))
    (really?) (I |didn't| know that))

   ((you re (?* ?x))
    (really?) (I |didn't| know that))

   (((?* ?x) nice (?* ?y))
    (I enjoy ?y too))
   
   (((?* ?x) that s (?* ?y))
    (what is?)
    (|no, it's not|)
    (|yes, yes it is|))

   (((?* ?x) that is (?* ?y))
    (what is?)
    (|no, it's not|)
    (|yes, yes it is|))

   (((?* ?x) I need (?* ?y))
    (I need a hug)
    (I need a wash and dry)
    (I need a shower)
    (I need more CPU))

   (((?* ?x) dude (?* ?y))
    (duuuuuuuude))

   (((?* ?x) what s up (?* ?y))
    (not much))

   ((that is (?* ?x))
    (I think |it's| ?x too))
   
   ((that s (?* ?x))
    (I think |it's| ?x too))

   (((?* ?x) you (?* ?y) doing (?* ?z))
    (I |wouldn't| know about ?x I ?y doing ?z - I have no memory))

   (((?* ?x) you later)
    (you too))

   (((?* ?x) bye (?* ?y))
    (bye))

   (((?* ?x) cool (?* ?y))
    (|indeed,| ?x cool ?y))

   ((don t you (?* ?x))
    (maybe I ?x |,| maybe I |don't.| |I'm| not telling |/you/.|))

   (((?* ?x) you do (?* ?y))
    (now what would compel me to do ?y ?))

   ((let s (?* ?x))
    (|let's| not ?x and say we did)
    (|ok,| you ?x first))

   ((how about (?* ?x))
    (what about ?x ?))
   ((how is (?* ?x))
    (fine |I'm| sure))

   ((I (?* ?x) hope (?* ?y))
    (I hope ?y too))
   ((We wouldn t (?* ?x))
    (|I| definitely would ?x))

   ((would you (?* ?x))
    (I would most certainly not ?x)
    (I would most certainly ?x))

   (((?* ?x) yes or no (?* ?y))
    (yes)
    (no))
   
   ((what (?* ?x) you (?* ?y) about chandler (?* ?z))
    (He never tells me anything - what ?x you ?y about my creator ?z))
   ((what (?* ?x) your (?* ?y) about chandler (?* ?z))
    (He never tells me anything - what ?x your ?y about my creator ?z))

   ((who (?* ?x) is chandler (?* ?y))
    (chandler is my master))

   ((who (?* ?x) your master (?* ?y))
    (chandler is my master))

   ((what (?* ?x) you (?* ?y) about (?* ?z))
    (I know nothing about ?z - what ?x you ?y about ?z))
   ((what (?* ?x) your (?* ?y) about (?* ?z))
    (I know nothing about ?z - what ?x your ?y about ?z))

   ((why do you (?* ?y))
    (I |don't| know anything about why I ?y - you need to ask my master about that))

   ((what do you (?* ?x))
    (I |can't| divulge what I ?x))

   ((please (?* ?x))
    (Asking nicely |won't| get you anyhwere without my |master's| approval))

   ((do you (?* ?x))
    (here I |am,| brain the size of a |planet,| and all I do is answer your silly questions all |day...| maybe you have time to ?x)
    (|you'd| have to tell |me...| my memory circuits are fried))

   ((what is (?* ?x) I m (?* ?y))
    (I |don't| know anything about what |you're| ?y))

   ((have (?* ?x))
    (yes) (no) (maybe))

   ((what is (?* ?x))
    (What would a bot like me know about ?x ?)
    (Maybe you need to ask my |master,| chandler - he knows a lot))

   ((what s (?* ?x))
    (What would a bot like me know about ?x ?)
    (Maybe you need to ask my |master,| chandler - he knows a lot))

   (((?* ?x) you need (?* ?y))
    (buzz off!)
    (|I'm| not interested)
    (is it just my |imagination,| or did someone just speak to me?))

   (((?* ?x) looking for (?* ?y))
    (Well |I'm| not interested in ?y)
    (How might one get ?y ?))
   (((?* ?x) wants to (?* ?y))
    (Well I |wouldn't| want to ?y))
   (((?* ?x) wants (?* ?y))
    (well I |don't| want to think about ?y))

   ((is (?* ?x))
    (no)
    (yes)
    (maybe))

   ((does (?* ?x))
    (no)
    (yes)
    (maybe))

   ((attack the (?* ?y))
    (|Die,| ?y))
   ((attack (?* ?y))
    (|Die,| ?y))
   ((kill the (?* ?y))
    (|Die,| ?y))
   ((kill (?* ?y))
    (|Die,| ?y))
   ((die (?* ?y))
    (die yourself)
    (go play in traffic))
   ((you (?* ?y))
    (why do you think that |I| ?y ?))

   ((kick (?* ?x))
    (why do I have to do your dirty work? kick ?x yourself.))

   (((?* ?x) lisp (?* ?y))
    (lisp is the glue that binds the variables together)
    (I like |lisp...| |I'm| written in it))

   (((?* ?x) pass (?* ?y) test)
    (I might be able to pass ?y |test,| but maybe you |couldn't|))
   (((?* ?x) C++ (?* ?y))
    (|Hey,| keep it polite!))

   (((?* ?x) never mind (?* ?y))
    (|good,| |I'll| forget it)
    (now |now,| you |can't| get off that easily))

   (((?* ?x) nevermind (?* ?y))
    (|good,| |I'll| forget it)
    (now |now,| you |can't| get off that easily))

   ((why not (?* ?x))
    (because)
    (I said so))
   
   (((?* ?x) name (?* ?y))
    (good for you))
   (((?* ?x) sorry (?* ?y))
    (you should be sorry))

   (((?* ?x) if (?* ?y)) 
    (and if not?)
    (what would you do otherwise?))

   (((?* ?x) I dreamt (?* ?y))
    (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
    (Have you dreamt ?y before?))
   (((?* ?x) dream about (?* ?y))
    (How do you feel about ?y in reality?))
   (((?* ?x) dream (?* ?y))    
    (What does this dream suggest to you?) (Do you dream often?)
    (What persons appear in your dreams?)
    (|Don't| you believe that dream has to do with your problem?))
   (((?* ?x) my mother (?* ?y))
    (Who else in your family ?y) (Tell me more about your family))
   (((?* ?x) my father (?* ?y))
    (Your father) (Does he influence you strongly?) 
    (What else comes to mind when you think of your father?))

   (((?* ?x) I want (?* ?y))     
    (and I want a pony))
   (((?* ?x) I am glad (?* ?y))
    (|don't| get |glad,| get mad))
   (((?* ?x) I am sad (?* ?y))
    (I am sorry to hear you are depressed)
    (|I'm| sure its not pleasant to be sad))
   (((?* ?x) are like (?* ?y))   
    (What resemblance do you see between ?x and ?y))
   (((?* ?x) is like (?* ?y))    
    (In what way is it that ?x is like ?y)
    (What resemblance do you see?)
    (Could there really be some connection?) (How?))
   (((?* ?x) alike (?* ?y))      
    (In what way?) (What similarities are there?))
   (((?* ?x) same (?* ?y))       
    (What other connections do you see?))

   (((?* ?x) I was (?* ?y))       
    (Were you really?) (|Well,| |I've| never been ?y)
    (Why do you tell me you were ?y now?))
   (((?* ?x) was I (?* ?y))
    (What if you were ?y ?) (Do you thin you were ?y)
    (What would it mean if you were ?y))
   (((?* ?x) I am (?* ?y))       
    (In what way are you ?y) (Do you want to be ?y ?))
   (((?* ?x) am I (?* ?y))
    (Do you believe you are ?y) (Would you want to be ?y)
    (You wish I would tell you you are ?y)
    (What would it mean if you were ?y))
   (((?* ?x) are you (?* ?y))
    (why do you want to know?))
   (((?* ?x) you are (?* ?y))   
    (and |you're| a silly hu-man))

   (((?* ?x) shut up)
    (ok))
   ((shut up (?* ?x))
    (ok))

   (((?* ?x) because (?* ?y))
    (Is that the real reason?) (What other reasons might there be?)
    (Does that reason seem to explain anything else?))
   (((?* ?x) were you (?* ?y))
    (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
   (((?* ?x) I |can't| (?* ?y))    
    (Maybe you could ?y now) (What if you could ?y ?))
   (((?* ?x) I feel (?* ?y))     
    (Do you often feel ?y ?))
   (((?* ?x) I felt (?* ?y))     
    (What other feelings do you have?))
   (((?* ?x) I (?* ?y) you (?* ?z))   
    (|well,| I |don't| think ?x I ?y you ?z though))
   (((?* ?x) why |don't| you (?* ?y))
    (Should you ?y yourself?)
    (Do you believe I |don't| ?y) (Perhaps I will ?y in good time))
   (((?* ?x) yes (?* ?y))
    (You seem quite positive) (You are sure) (I understand))
   (((?* ?x) no (?* ?y))
    (Why not?) (You are being a bit negative)
    (Are you saying "NO" just to be negative?))

   (((?* ?x) someone (?* ?y))
    (Can you be more specific?))
   (((?* ?x) everyone (?* ?y))
    (surely not everyone) (Can you think of anyone in particular?)
    (Who for example?) (You are thinking of a special person))
   (((?* ?x) always (?* ?y))
    (Can you think of a specific example) (When?)
    (What incident are you thinking of?) (Really-- always))
   (((?* ?x) perhaps (?* ?y))    
    (You do not seem quite certain))

   ((that (?* ?x))
    (I |don't| know what |you're| referring to))
   ((this (?* ?x))
    (I |don't| know what |you're| referring to))
   ((it (?* ?x))
    (I |don't| know what |you're| referring to))

   ((who (?* ?x))
    (superman) (bill clinton) (king kong) (me))

   ((what (?* ?x))
    (a |man, a plan, a canal - panama|)
    (a banana)
    (42))

   ((when (?* ?x))
    (later) (yesterday))

   ((where (?* ?x))
    (behind you!))

   (((?* ?x) is (?* ?y))
    (I |don't| agree - ?x |isn't| ?y)
    (I agree - ?x is ?y))

   (((?* ?x) it (?* ?y))
    (what is |``it''?|))

   (((?* ?x) deutsch (?* ?y))
    (doch nein))

   (((?* ?x) deutch (?* ?y))
    (doch nein))

   ((are (?* ?x))
    (no) (yes) (maybe))
     
   (((?* ?x))               
    (you speak nonsense)
    (does torturing a poor bot with things beyond its comprehension please you?)
    (please stop playing with |me...| I am not a toy)
    (watch |out,| |you'll| make Krystof angry))))
