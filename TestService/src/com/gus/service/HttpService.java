package com.gus.service;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/mydev")
public class HttpService {
	
	public final static Map<Long,FutureTask<String>> MYDEV_THREAD_INTTERUPT_FLAG = new HashMap<Long,FutureTask<String>>();
	
	@Autowired
	private  HttpServletRequest request;
	
	//MYDEV_THREAD_INTTERUPT_FLAG 保证全局唯一
	@RequestMapping("/s1")
	//打断接口
	//http://127.0.0.1:8081/mydev/s1?MYDEV_THREAD_INTTERUPT_FLAG=100
	public String s1() throws Exception {
		
		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));
		FutureTask<String> future = MYDEV_THREAD_INTTERUPT_FLAG.get(id);
		future.cancel(true);
		
		return id +" THREAD_INTTERUPT...";
	}

	@RequestMapping("/s2")
	//模拟爬虫接口
	//http://127.0.0.1:8081/mydev/s2?MYDEV_THREAD_INTTERUPT_FLAG=100
	public String s2() {
		
		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));
		
		Callable<String> callable = new Callable<String>() {
			public String call() throws Exception {
				try {
					for (int i = 0; i < 1000000000; i++) {
						System.out.println(i);
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
		
		MYDEV_THREAD_INTTERUPT_FLAG.put(id, future);
		
		new Thread(future).start();
		
		try {
			return future.get();
		} catch (InterruptedException e) {
			return e.getMessage();

		} catch (ExecutionException e) {
			return e.getMessage();
		}
	}
}
