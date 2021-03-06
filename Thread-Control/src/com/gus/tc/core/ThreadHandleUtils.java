package com.gus.tc.core;


import javax.servlet.http.HttpServletRequest;

public class ThreadHandleUtils {

	public static void SinglThreadHandle(HttpServletRequest request) throws Exception {

		if (request.getParameter("thread_intterupt_id") == null) {
			throw new Exception("thread_intterupt_id is null");
		}

		Long id = Long.parseLong(request.getParameter("thread_intterupt_id"));
		TcBean tb = new TcBean();
		tb.getThreads().add(Thread.currentThread());
		Constants.mtc.put(id, tb);
	}

	public static void FutureHandle(HttpServletRequest request,TcBean tb) throws Exception {
		
		Thread.sleep(1000);

		if (request.getParameter("thread_intterupt_id") == null) {
			throw new Exception("thread_intterupt_id is null");
		}
		Long id = Long.parseLong(request.getParameter("thread_intterupt_id"));
		Constants.mtc.put(id, tb);
	}

}
