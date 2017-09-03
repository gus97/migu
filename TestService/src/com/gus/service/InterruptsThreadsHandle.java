package com.gus.service;

import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/InterruptsThreadsHandle")
public class InterruptsThreadsHandle {

	@Autowired
	private HttpServletRequest request;

	@SuppressWarnings("deprecation")
	@RequestMapping("/stop")
	public String stopHandle() throws Exception {

		if (request.getParameter("thread_intterupt_id") == null) {

			return "thread_intterupt_id is null";
		}

		Long id = Long.parseLong(request.getParameter("thread_intterupt_id"));
		
	
		//一切都是以停止线程为目的，线程池亦是如此，停止了所有线程还要停止线程池，先停止线程池在停止线程
		if (Constants.mtc.containsKey(id)) {
			if (Constants.mtc.get(id).getExecutorService() != null) {
				Constants.mtc.get(id).getExecutorService().shutdownNow();
			}
			
			
			List<Thread> list = Constants.mtc.get(id).getThreads();
			for (Iterator<Thread> iterator = list.iterator(); iterator.hasNext();) {
				Thread thread = (Thread) iterator.next();
				thread.stop();
			}
	

			Constants.mtc.remove(id);
		}

		return id + " stopHandle finshed";

	}

	@RequestMapping("/intterupt")
	public String intteruptHandle() throws Exception {

		if (request.getParameter("thread_intterupt_id") == null) {

			return "thread_intterupt_id is null";
		}

		Long id = Long.parseLong(request.getParameter("thread_intterupt_id"));

		return id + " intteruptHandle finshed";

	}

}
