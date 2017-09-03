package com.gus.tc.demo;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gus.tc.core.TcBean;
import com.gus.tc.core.ThreadHandleUtils;

@RestController
@RequestMapping("/mydev")
public class HttpService4Interrupted {

	@Autowired
	private HttpServletRequest request;

	//http://127.0.0.1:8081/mydev/Interrupted?thread_intterupt_id=100
	@RequestMapping("/Interrupted")
	public String interruptedMod() throws Exception {

		TcBean tb = new TcBean();
		List<Thread> ths = new ArrayList<Thread>();
		tb.setThreads(ths);

		Callable<String> callable = new Callable<String>() {

			public String call() throws Exception {

				ths.add(Thread.currentThread());

				try {
					for (int i = 0; i < 100000000; i++) {
						if (Thread.currentThread().isInterrupted()) {
							throw new InterruptedException();
						}
					}
					return new Random().nextInt(1000) + "";
				} catch (InterruptedException e) {
					return "主动打断 callable=============================>>>";
				}
			}
		};

		FutureTask<String> future = new FutureTask<String>(callable);

		new Thread(future).start();
		
		ThreadHandleUtils.FutureHandle(request,tb);

		return future.get();
	}

}
