package com.gus.service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/mydev")
public class HttpService4Future {
	
	@Autowired
	private HttpServletRequest request;

	//http://127.0.0.1:8081/mydev/FutureTask?thread_intterupt_id=100
	@RequestMapping("/FutureTask")
	public String batchUpdate() throws Exception {
		
		TcBean tb = new TcBean();
		List<Thread> ths = new ArrayList<Thread>(); 
		tb.setThreads(ths);
		
		//你可能的业务情形之一
		ExecutorService service = Executors.newFixedThreadPool(2);
		
		tb.setExecutorService(service);

		Future<String> future1 = service.submit(new Callable<String>() {
			@Override
			public String call() throws Exception {
				//耦合代码
				ths.add(Thread.currentThread());
				//你的具体业务之一
				Thread.sleep(10000);
				return "Hello1";
			}
		});

		Future<String> future2 = service.submit(new Callable<String>() {
			@Override
			public String call() throws Exception {
				//耦合代码
				ths.add(Thread.currentThread());
				//你的具体业务之一
				Thread.sleep(10000);
				return "Hello2";
			}
		});
		
		//需要耦合的地方
		Thread.sleep(1000);

		ThreadHandleUtils.FutureHandle(request,tb);
		
		System.out.println(future1.get());
		System.out.println(future2.get());
		
		return "0000";
	}

}
