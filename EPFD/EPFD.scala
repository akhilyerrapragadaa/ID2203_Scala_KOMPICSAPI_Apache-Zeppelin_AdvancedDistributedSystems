
//Define EPFD Implementation
class EPFD(epfdInit: Init[EPFD]) extends ComponentDefinition {

  //EPFD subscriptions
  val timer = requires[Timer];
  val pLink = requires[PerfectLink];
  val epfd = provides[EventuallyPerfectFailureDetector];

  // EPDF component state and initialization
  
  //configuration parameters
  val self = epfdInit match {case Init(s: Address) => s};
 // println(self) //Self address
  val topology = cfg.getValue[List[Address]]("epfd.simulation.topology");
  //println(topology) //Everyones address
  val delta = cfg.getValue[Long]("epfd.simulation.delay");
  //println(delta) // 100
  
  //mutable state
  var period = cfg.getValue[Long]("epfd.simulation.delay");
  //println(period) //100
  var alive = Set(cfg.getValue[List[Address]]("epfd.simulation.topology"): _*);
  //println(alive) //All alive processes
  var suspected = Set[Address](); 
  //println(suspected)//Empty
  var seqnum = 0;
  var count =0;

  //No timer started yet
  def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    //println(scheduledTimeout)
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

   //Start
  //EPFD event handlers
  ctrl uponEvent {
    case _: Start =>  {
        /* WRITE YOUR CODE HERE  */
        startTimer(100)
    }
  }
  
  //Triggered from start timer
  timer uponEvent {
    case CheckTimeout(_) =>  {
        
      if (!alive.intersect(suspected).isEmpty) {
        /* WRITE YOUR CODE HERE  */
        period = period + delta
      }
      
      seqnum = seqnum + 1;
     
      for (p <- topology) {
         
         count+=1
        if (!alive.contains(p) && !suspected.contains(p)) {
         println(count)
         
         suspected += p
           /* WRITE YOUR CODE HERE  */
           trigger(Suspect(p) -> epfd);
        } 
        
        else if (alive.contains(p) && suspected.contains(p)) {
          suspected = suspected - p;
          trigger(Restore(p) -> epfd);
        }
        trigger(PL_Send(p, HeartbeatRequest(seqnum)) -> pLink);
      }
      alive = Set[Address]();
      startTimer(period);
    }
  }

  pLink uponEvent {
    case PL_Deliver(src, HeartbeatRequest(seq)) =>  {
      /* WRITE YOUR CODE HERE  */   
     trigger(PL_Send(src, HeartbeatReply(seq)) -> pLink);
    }
    case PL_Deliver(src, HeartbeatReply(seq)) => {
      /* WRITE YOUR CODE HERE  */
      if((seq == seqnum) || suspected.contains(src)){
          alive += src
      }
    }
  }
};


