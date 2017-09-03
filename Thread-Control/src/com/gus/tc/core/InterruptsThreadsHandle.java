package com.gus.tc.core;

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

	//暂时以Thread和Future两种模式支持
	@RequestMapping("/intterupt")
	public String intteruptHandle() throws Exception {

		if (request.getParameter("thread_intterupt_id") == null) {

			return "thread_intterupt_id is null";
		}

		Long id = Long.parseLong(request.getParameter("thread_intterupt_id"));
		
		if (Constants.mtc.containsKey(id)) {
			if (Constants.mtc.get(id).getExecutorService() != null) {
				Constants.mtc.get(id).getExecutorService().shutdownNow();
			}

			List<Thread> list = Constants.mtc.get(id).getThreads();
			for (Iterator<Thread> iterator = list.iterator(); iterator.hasNext();) {
				Thread thread = (Thread) iterator.next();
				thread.interrupt();
			}

			Constants.mtc.remove(id);
		}
		

		return id + " intteruptHandle finshed";

	}

}
