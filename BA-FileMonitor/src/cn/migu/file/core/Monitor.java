package cn.migu.file.core;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.management.ManagementFactory;
import java.util.HashMap;
import java.util.Map;

import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

public class Monitor {
	
	private static Monitor s_instance = new Monitor();
	
	private static Map<String, MetricInfo> map = new HashMap<String, MetricInfo>();
	
	//static String RMI_URL = "service:jmx:rmi:///jndi/rmi://127.0.0.1:9999/jmxrmi";
	
	static{
		MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
		MetricMBean mbean = new MetricMBean();
		mbean.setMetric(map);
		try {
			mbs.registerMBean(mbean, new ObjectName("miguMonitor:jmx=miguMbean"));
		} catch (InstanceAlreadyExistsException | MBeanRegistrationException | NotCompliantMBeanException
				| MalformedObjectNameException e) {
			e.printStackTrace();
		}
	}
	
	private Monitor(){}
	
	public Monitor getInstance() {
		return s_instance;
	}
	
//	public static void initJmxServer() throws Exception{
//		MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
//		MetricMBean mbean = new MetricMBean();
////		LocateRegistry.createRegistry(port);
////		JMXServiceURL serviceURL = new JMXServiceURL(RMI_URL.replace("9999", String.valueOf(port)));
////		JMXConnectorServer jcs = JMXConnectorServerFactory.newJMXConnectorServer(serviceURL, null, mbs);
////        System.out.println("begin rmi start");
//		mbean.setMetric(map);
//		mbs.registerMBean(mbean, new ObjectName("miguMonitor:jmx=miguMbean"));
////		jcs.start();
//	}
	
	public static void put(String key,MetricInfo info){
		
		if(!map.containsKey(key)){
			map.put(key, info);
		}else{
			MetricInfo m = map.get(key);
			m.setFileNum(m.getFileNum()+info.getFileNum());
			m.setRecordNum(m.getRecordNum()+info.getRecordNum());
			map.put(key,m);
		}
		
	}
	
	public static void remove(String key){
		map.remove(key);
	}
	public static void main(String[] args) {
		
	}
	
	static String getException(String message,Throwable cause){
		StringWriter writer = new StringWriter(2048);

		if (message != null) {
			writer.write(message);
			writer.write(System.getProperty("line.separator"));
		}
		cause.printStackTrace(new PrintWriter(writer));
		String detailMessage = writer.toString();
		return detailMessage;
	}
}
