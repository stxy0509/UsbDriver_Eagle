#include "wdm.h"
#include "usbdi.h"
#include "usbdlib.h"
#include "stusb.h"

NTSTATUS
Stusb_Create(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    )
{
	NTSTATUS ntStatus;
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;
	
	KdPrint(("=> Stusb_Create()\n"));
	// increment the open handle counter
	pdx->OpenHandles++;
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	ntStatus = Irp->IoStatus.Status;
	IoCompleteRequest(Irp,IO_NO_INCREMENT);
	KdPrint(("<= Stusb_Create()\n"));
	return ntStatus;
}


NTSTATUS
Stusb_Close(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    )
{
	NTSTATUS ntStatus;
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;
	
	KdPrint(("=> Stusb_Close()\n"));
	// decrement the open handle counter
	pdx->OpenHandles--;
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;
	ntStatus = Irp->IoStatus.Status;
	IoCompleteRequest(Irp,IO_NO_INCREMENT);
	KdPrint(("<= Stusb_Close()\n"));
	return ntStatus;
}


NTSTATUS
Stusb_DispatchPnp(
   IN PDEVICE_OBJECT fdo,
   IN PIRP           Irp
   )
{
	PIO_STACK_LOCATION irpStack;
	PDEVICE_EXTENSION  pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;
	NTSTATUS ntStatus;
	ULONG    fcn;

	KdPrint(("=> Stusb_DispatchPnp()\n"));
	if(!LockDevice(fdo))
	{
		return CompleteRequest(Irp, STATUS_DELETE_PENDING, 0);
	}

	// Get a pointer to the current location in the Irp. This is 
	// where the function codes and parameters are located
	irpStack = IoGetCurrentIrpStackLocation(Irp);
	ASSERT(irpStack->MajorFunction == IRP_MJ_PNP);

	fcn = irpStack->MinorFunction;
	switch(fcn)
	{
		case IRP_MN_START_DEVICE:
		{
			KdPrint(("IRP_MN_START_DEVICE\n"));
			ntStatus = Stusb_HandleStartDevice(fdo,Irp);
			pdx->Started = (ntStatus == STATUS_SUCCESS);
			break;
		}
		case IRP_MN_STOP_DEVICE:
		{
			KdPrint(("IRP_MN_STOP_DEVICE\n"));
			// first pass the request down the stack
			Stusb_DefaultPnpHandler(fdo,Irp);
			ntStatus = Stusb_StopDevice(fdo);
			break; 
		}
		case IRP_MN_REMOVE_DEVICE:
		{
			KdPrint(("IRP_MN_REMOVE_DEVICE\n"));
			ntStatus = Stusb_HandleRemoveDevice(fdo,Irp);
			break;
		}
		case IRP_MN_QUERY_CAPABILITIES:
		{
			PDEVICE_CAPABILITIES pdc;
			KdPrint(("IRP_MN_QUERY_CAPABILITIES\n"));
			pdc = irpStack->Parameters.DeviceCapabilities.Capabilities;
			
			// Check to be sure we know how to handle this version of the 
			// capabilities structure
			if(pdc->Version < 1)
			{
				ntStatus = Stusb_DefaultPnpHandler(fdo, Irp);
				break;
			}
			ntStatus = ForwardAndWait(fdo, Irp);
			if(NT_SUCCESS(ntStatus))
			{						
				pdc = irpStack->Parameters.DeviceCapabilities.Capabilities;
				// setting this field prevents NT5 from notifying the user
				// when the device is removed.
				pdc->SurpriseRemovalOK = TRUE;
			}				
			ntStatus = CompleteRequest(Irp, ntStatus, Irp->IoStatus.Information);
			break;
		}
		case IRP_MN_SURPRISE_REMOVAL:
		{
			KdPrint(("IRP_MN_SURPRISE_REMOVAL\n"));
			ntStatus = Stusb_DefaultPnpHandler(fdo, Irp);
			Stusb_SurpriseRemoveDevice(fdo);
			break;
		}
		// All other PNP IRP's are just passed down the stack 
		// by the default handler
		default:
		{
			KdPrint(("Passing down unhandled PnP IOCTL 0x%x\n", fcn));
			ntStatus = Stusb_DefaultPnpHandler(fdo, Irp);
		}
	}

	if(fcn != IRP_MN_REMOVE_DEVICE )
		UnlockDevice(fdo);

	KdPrint(("<= Stusb_DispatchPnp() %x\n", ntStatus));
	return ntStatus;
}


NTSTATUS
DriverEntry(
	IN PDRIVER_OBJECT DriverObject,
	IN PUNICODE_STRING RegisteryPath)
{
	KdPrint(("=> DriverEntry()\n"));
	KdPrint(("RegisteryPath :%ws\n",RegisteryPath->Buffer));
	DriverObject->DriverUnload = Stusb_Unload;
	DriverObject->DriverExtension->AddDevice = Stusb_PnPAddDevice;
	
	DriverObject->MajorFunction[IRP_MJ_CREATE] = Stusb_Create;
	DriverObject->MajorFunction[IRP_MJ_CLOSE] = Stusb_Close;
	DriverObject->MajorFunction[IRP_MJ_PNP] = Stusb_DispatchPnp;
	DriverObject->MajorFunction[IRP_MJ_POWER] = Stusb_DispatchPower;
	DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = Stusb_ProcessIOCTL;
	KdPrint(("<= DriverEntry()\n"));
	return STATUS_SUCCESS;
}


NTSTATUS
Stusb_CreateDeviceObject(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT *DeviceObject,
    LONG Instance
    )
{
	NTSTATUS ntStatus;
	WCHAR deviceLinkBuffer[]  = L"\\DosDevices\\Bgang-0";
	UNICODE_STRING deviceLinkUnicodeString;
	WCHAR deviceNameBuffer[]  = L"\\Device\\Bgang-0";
	UNICODE_STRING deviceNameUnicodeString;
	PDEVICE_EXTENSION pdx;

	KdPrint(("=> Stusb_CreateDeviceObject instance = %d\n", Instance));

	// fix up device names based on Instance
	deviceLinkBuffer[18] = (USHORT) ('0' + Instance);
	deviceNameBuffer[14] = (USHORT) ('0' + Instance);

	KdPrint(("Create Device name (%ws)\n", deviceNameBuffer));

	RtlInitUnicodeString(&deviceNameUnicodeString,
						 deviceNameBuffer);
	ntStatus = IoCreateDevice(DriverObject,
							  sizeof (DEVICE_EXTENSION),
							  &deviceNameUnicodeString,
							  FILE_DEVICE_UNKNOWN,
							  0,
							  FALSE,
							  DeviceObject);
	if(NT_SUCCESS(ntStatus))
	{
		// Initialize our device extension
		pdx = (PDEVICE_EXTENSION) ((*DeviceObject)->DeviceExtension);
		RtlCopyMemory(pdx->DeviceLinkNameBuffer,
				   deviceLinkBuffer,
				   sizeof(deviceLinkBuffer));
		pdx->OpenHandles = 0;
		pdx->ConfigurationHandle = NULL;
		pdx->DeviceDescriptor = NULL;
		pdx->NeedCleanup = FALSE;
		pdx->Started = FALSE;

		// Initialize our interface
		pdx->Interface = NULL;

		RtlInitUnicodeString(&deviceLinkUnicodeString,
							 deviceLinkBuffer);
		KdPrint(("Create DosDevice name (%ws)\n", deviceLinkBuffer));

		ntStatus = IoCreateSymbolicLink(&deviceLinkUnicodeString,
									    &deviceNameUnicodeString);
		if(NT_SUCCESS(ntStatus))
		{
			KdPrint(("Create SymbolicLink OK.\n"));
			pdx->NeedCleanup = TRUE;
		}
	}

	KdPrint(("<= Stusb_CreateDeviceObject (%x)\n", ntStatus));
	return ntStatus;
}



VOID
Stusb_Unload(
    IN PDRIVER_OBJECT DriverObject
    )
{
	KdPrint(("=> Stusb_Unload()!\n"));
	
	KdPrint(("<= Stusb_Unload()!\n"));
}


NTSTATUS
Stusb_PnPAddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
    )
{
	NTSTATUS                ntStatus = STATUS_SUCCESS;
	PDEVICE_OBJECT          deviceObject = NULL;
	PDEVICE_EXTENSION       pdx;
	int instance;

	KdPrint(("=> Stusb_PnPAddDevice\n"));

#define MAX_STUSB_DEVICES 8

	// create our functional device object (FDO).  This driver supports multiple stusb 
	// devices.This loop will look for an available instance number.  Keep incrementing 
	// the instance until a call to stusb_CreateDeviceObject succeeds.
	// every time,test from instance 0 to max
	instance = 0;
	do
	{
		ntStatus = Stusb_CreateDeviceObject(DriverObject, 
											&deviceObject, 
											instance);
		instance++;
	} while (!NT_SUCCESS(ntStatus) && (instance < MAX_STUSB_DEVICES));

	if(NT_SUCCESS(ntStatus))
	{
		pdx = deviceObject->DeviceExtension;
		// store away the Physical device Object
		pdx->PhysicalDeviceObject = PhysicalDeviceObject;
		// Non plug and play drivers usually create the device object in
		// driver entry, and the I/O manager autimatically clears this flag.
		// Since we are creating the device object ourselves in response to 
		// a PnP START_DEVICE IRP, we need to clear this flag ourselves.
		deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

		// This driver uses direct I/O for read/write requests
		deviceObject->Flags |= DO_DIRECT_IO;

		deviceObject->Flags |= DO_POWER_PAGABLE;

		// Attach to the StackDeviceObject.  This is the device object that what we 
		// use to send Irps and Urbs down the USB software stack
		pdx->StackDeviceObject =
			IoAttachDeviceToDeviceStack(deviceObject, PhysicalDeviceObject);
		
		ASSERT (pdx->StackDeviceObject != NULL);

		pdx->LastFailedUrbStatus = 0;

		pdx->Configed = 0;
		
		// locked until RemoveDevice
		pdx->usage = 1;		
		// set when use count drops to zero		
		KeInitializeEvent(&pdx->evRemove,
						  NotificationEvent,
						  FALSE);   
	}

	KdPrint(("<= Stusb_PnPAddDevice (%x)\n", ntStatus));
	return ntStatus;
}


BOOLEAN LockDevice(
   IN PDEVICE_OBJECT fdo
   )
{
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;

	// Increment use count on our device object
	LONG usage = InterlockedIncrement(&pdx->usage);

	// AddDevice initialized the use count to 1, so it ought to be bigger than
	// one now. HandleRemoveDevice sets the "removing" flag and decrements the
	// use count, possibly to zero. So if we find a use count of "1" now, we
	// should also find the "removing" flag set.

	ASSERT(usage > 1 || pdx->removing);

	// If device is about to be removed, restore the use count and return FALSE.
	// If we're in a race with HandleRemoveDevice (maybe running on another CPU),
	// the sequence we've followed is guaranteed to avoid a mistaken deletion of
	// the device object. If we test "removing" after HandleRemoveDevice sets it,
	// we'll restore the use count and return FALSE. In the meantime, if
	// HandleRemoveDevice decremented the count to 0 before we did our increment,
	// its thread will have set the remove event. Otherwise, we'll decrement to 0
	// and set the event. Either way, HandleRemoveDevice will wake up to finish
	// removing the device, and we'll return FALSE to our caller.
	// 
	// If, on the other hand, we test "removing" before HandleRemoveDevice sets it,
	// we'll have already incremented the use count past 1 and will return TRUE.
	// Our caller will eventually call UnlockDevice, which will decrement the use
	// count and might set the event HandleRemoveDevice is waiting on at that point.

	if(pdx->removing)
	{
		if(InterlockedDecrement(&pdx->usage) == 0)
			KeSetEvent(&pdx->evRemove, 0, FALSE);
		return FALSE;
	}
	return TRUE;
}


VOID UnlockDevice(
   PDEVICE_OBJECT fdo
   )
{
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	LONG usage = InterlockedDecrement(&pdx->usage);

	ASSERT(usage >= 0);
	if(usage == 0)
	{							// removing device
		ASSERT(pdx->removing);	// HandleRemoveDevice should already have set this
		KeSetEvent(&pdx->evRemove, 0, FALSE);
	}
}

NTSTATUS
Stusb_RemoveDevice(
    IN  PDEVICE_OBJECT fdo
    )
{
	PDEVICE_EXTENSION pdx;
	NTSTATUS ntStatus = STATUS_SUCCESS;
	pdx = fdo->DeviceExtension;
	
	KdPrint(("=> Stusb_RemoveDevice\n"));
	Stusb_ReleaseDeviceMem(fdo);
	IoDetachDevice(pdx->StackDeviceObject);
	IoDeleteDevice(fdo);
	KdPrint(("<= Stusb_RemoveDevice (%x)\n", ntStatus));
	return ntStatus;
}

NTSTATUS
Stusb_SurpriseRemoveDevice(
    IN  PDEVICE_OBJECT fdo
    )
{
	PDEVICE_EXTENSION pdx;
	NTSTATUS ntStatus = STATUS_SUCCESS;

	KdPrint(("=> Stusb_SurpriseRemoveDevice\n"));
	pdx = fdo->DeviceExtension;
	Stusb_AbortAllPipes(fdo);
	Stusb_ReleaseDeviceMem(fdo);
	KdPrint(("<= Stusb_SurpriseRemoveDevice (%x)\n", ntStatus));
	return ntStatus;
}

NTSTATUS
Stusb_HandleRemoveDevice(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   )
{	
	NTSTATUS ntStatus;
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;

	// set the removing flag to prevent any new I/O's
	pdx->removing = TRUE;
	Stusb_AbortAllPipes(fdo);
	UnlockDevice(fdo);			// once for LockDevice at start of dispatch
	UnlockDevice(fdo);			// once for initialization during AddDevice
	KeWaitForSingleObject(&pdx->evRemove, Executive, KernelMode, FALSE, NULL);
	Stusb_RemoveDevice(fdo);
	ntStatus = Stusb_DefaultPnpHandler(fdo, Irp);
	return ntStatus;			// lower-level completed IoStatus already
}

NTSTATUS
CompleteRequest(
   IN PIRP Irp,
   IN NTSTATUS status,
   IN ULONG info
   )
{
	Irp->IoStatus.Status = status;
	Irp->IoStatus.Information = info;
	IoCompleteRequest(Irp, IO_NO_INCREMENT);
	return status;
}

NTSTATUS
Stusb_DefaultPnpHandler(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   )
{
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;

	IoSkipCurrentIrpStackLocation(Irp);
	return IoCallDriver(pdx->StackDeviceObject, Irp);
}


NTSTATUS
ForwardAndWait(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   )
{
	KEVENT event;
	PDEVICE_EXTENSION pdx = (PDEVICE_EXTENSION) fdo->DeviceExtension;
	NTSTATUS ntStatus;

    ASSERT(KeGetCurrentIrql() == PASSIVE_LEVEL);
	
    // Initialize a kernel event object to use in waiting for the 
	// lower-level driver to finish processing the object. 
	KeInitializeEvent(&event, NotificationEvent, FALSE);

	IoCopyCurrentIrpStackLocationToNext(Irp);
	IoSetCompletionRoutine(Irp, 
		(PIO_COMPLETION_ROUTINE)OnRequestComplete,
		(PVOID) &event, 
		TRUE, TRUE, TRUE);
	ntStatus = IoCallDriver(pdx->StackDeviceObject, Irp);
	if(ntStatus == STATUS_PENDING)
	{
		KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
		ntStatus = Irp->IoStatus.Status;
    }

	return ntStatus;
}


NTSTATUS 
OnRequestComplete(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp,
   IN PKEVENT pev
   )
{
	KeSetEvent(pev, 0, FALSE);
	return STATUS_MORE_PROCESSING_REQUIRED;
}

NTSTATUS
Stusb_HandleStartDevice(
   IN PDEVICE_OBJECT fdo,
   IN PIRP Irp
   )
{
	NTSTATUS ntStatus;
	// First let all lower-level drivers handle this request.
	ntStatus = ForwardAndWait(fdo, Irp);
	if(!NT_SUCCESS(ntStatus))
	{
		return CompleteRequest(Irp, ntStatus, Irp->IoStatus.Information);
	}
	// now do whatever we need to do to start the device
	ntStatus = Stusb_StartDevice(fdo);

	return CompleteRequest(Irp, ntStatus, 0);
}

NTSTATUS
Stusb_StartDevice(
    IN  PDEVICE_OBJECT fdo
    )
{
    PDEVICE_EXTENSION pdx;
    NTSTATUS ntStatus;
    PUSB_DEVICE_DESCRIPTOR deviceDescriptor = NULL;
    PURB urb;
    ULONG siz;

    KdPrint (("=> Stusb_StartDevice\n"));

    pdx = fdo->DeviceExtension;
    // Get some memory from then non paged pool (fixed, locked system memory)
    // for use by the USB Request Block (urb) for the specific USB Request we
    // will be performing below (a USB device request).
    urb = ExAllocatePoolWithTag( NonPagedPool,
						sizeof(struct _URB_CONTROL_DESCRIPTOR_REQUEST),
						' dmW');
    if(urb) 
    {
        siz = sizeof(USB_DEVICE_DESCRIPTOR);
        // Get some non paged memory for the device descriptor contents
        deviceDescriptor = ExAllocatePoolWithTag(NonPagedPool,
                                          siz,' dmW');
        if(deviceDescriptor) 
        {
            // Use a macro in the standard USB header files to build the URB
            UsbBuildGetDescriptorRequest(urb,
                                        (USHORT)sizeof (struct _URB_CONTROL_DESCRIPTOR_REQUEST),
                                         USB_DEVICE_DESCRIPTOR_TYPE,
                                         0,
                                         0,
                                         deviceDescriptor,
                                         NULL,
                                         siz,
                                         NULL);
            // Get the device descriptor
            ntStatus = Stusb_CallUSBD(fdo, urb);
            // Dump out the descriptor info to the debugger
            if(NT_SUCCESS(ntStatus)) 
            {
                KdPrint(("Device Descriptor = %x, len %x\n",
                               deviceDescriptor,
                               urb->UrbControlDescriptorRequest.TransferBufferLength));
                KdPrint(("Stusb Device Descriptor:\n"));
                KdPrint(("-------------------------\n"));
                KdPrint(("bLength %d\n", deviceDescriptor->bLength));
                KdPrint(("bDescriptorType 0x%x\n", deviceDescriptor->bDescriptorType));
                KdPrint(("bcdUSB 0x%x\n", deviceDescriptor->bcdUSB));
                KdPrint(("bDeviceClass 0x%x\n", deviceDescriptor->bDeviceClass));
                KdPrint(("bDeviceSubClass 0x%x\n", deviceDescriptor->bDeviceSubClass));
                KdPrint(("bDeviceProtocol 0x%x\n", deviceDescriptor->bDeviceProtocol));
                KdPrint(("bMaxPacketSize0 0x%x\n", deviceDescriptor->bMaxPacketSize0));
                KdPrint(("idVendor 0x%x\n", deviceDescriptor->idVendor));
                KdPrint(("idProduct 0x%x\n", deviceDescriptor->idProduct));
                KdPrint(("bcdDevice 0x%x\n", deviceDescriptor->bcdDevice));
                KdPrint(("iManufacturer 0x%x\n", deviceDescriptor->iManufacturer));
                KdPrint(("iProduct 0x%x\n", deviceDescriptor->iProduct));
                KdPrint(("iSerialNumber 0x%x\n", deviceDescriptor->iSerialNumber));
                KdPrint(("bNumConfigurations 0x%x\n", deviceDescriptor->bNumConfigurations));
            }
        } 
        else 
        {
            ntStatus = STATUS_NO_MEMORY;
        }

        if(NT_SUCCESS(ntStatus)) 
        {
            // Put a ptr to the device descriptor in the device extension for easy
            // access (like a "cached" copy).  We will free this memory when the
            // device is removed.  See the "Stusb_RemoveDevice" code.
            pdx->DeviceDescriptor = deviceDescriptor;
            pdx->Stopped = FALSE;
        } 
        else if(deviceDescriptor) 
        {
            // If the bus transaction failed, then free up the memory created to hold
            // the device descriptor, since the device is probably non-functional
            ExFreePool(deviceDescriptor);
            pdx->DeviceDescriptor = NULL;
        }
        ExFreePool(urb);
    } 
    else 
    {
        // Failed getting memory for the Urb 
        ntStatus = STATUS_NO_MEMORY;
    }

    // If the Get_Descriptor call was successful, then configure the 
    // device.
    if(NT_SUCCESS(ntStatus)) 
    {
        ntStatus = Stusb_ConfigureDevice(fdo);
    }

    KdPrint(("<= Stusb_StartDevice (%x)\n", ntStatus));
    return ntStatus;
}

NTSTATUS
Stusb_CallUSBD(
    IN PDEVICE_OBJECT fdo,
    IN PURB Urb
    )
{
	NTSTATUS ntStatus, status = STATUS_SUCCESS;
	PDEVICE_EXTENSION pdx;
	PIRP irp;
	KEVENT event;
	IO_STATUS_BLOCK ioStatus;
	PIO_STACK_LOCATION nextStack;

	KdPrint(("=> Stusb_CallUSBD\n"));

	pdx = fdo->DeviceExtension;
	// issue a synchronous request (see notes above)
	KeInitializeEvent(&event, NotificationEvent, FALSE);
	irp = IoBuildDeviceIoControlRequest(
			 IOCTL_INTERNAL_USB_SUBMIT_URB,
			 pdx->StackDeviceObject,
			 NULL,
			 0,
			 NULL,
			 0,
			 TRUE, /* INTERNAL */
			 &event,
			 &ioStatus);

	// Prepare for calling the USB driver stack
	nextStack = IoGetNextIrpStackLocation(irp);
	ASSERT(nextStack != NULL);

	// Set up the URB ptr to pass to the USB driver stack
	nextStack->Parameters.Others.Argument1 = Urb;

	KdPrint(("Calling USB Driver Stack\n"));

	// Call the USB class driver to perform the operation.  If the returned 
	// status is PENDING, wait for the request to complete.
	ntStatus = IoCallDriver(pdx->StackDeviceObject,
						    irp);
	KdPrint(("return from IoCallDriver USBD %x\n", ntStatus));
	if(ntStatus == STATUS_PENDING)
	{
		KdPrint(("Wait for single object\n"));
		status = KeWaitForSingleObject(
					&event,
					Suspended,
					KernelMode,
					FALSE,
					NULL);
		KdPrint(("Wait for single object, returned %x\n", status));
	}
	else
	{
		ioStatus.Status = ntStatus;
	}

	KdPrint (("URB status = %x status = %x irp status %x\n",
		Urb->UrbHeader.Status, status, ioStatus.Status));

	// USBD maps the error code for us.  USBD uses error codes in its URB
	// structure that are more insightful into USB behavior. In order to
	// match the NT Status codes, USBD maps its error codes into more general NT
	// error categories so higher level drivers can decipher the error codes
	// based on standard NT error code definitions.
	ntStatus = ioStatus.Status;

	// If the URB status was not USBD_STATUS_SUCCESS, we save a copy of the
	// URB status in the device extension.  After a failure, another IOCTL,
	// IOCTL_Stusb_GET_LAST_ERROR can be used to retrieve the URB status
	// for the most recently failed URB.  Of course, this status gets
	// overwritten by subsequent failures, but it's better than nothing.
	if (!(USBD_SUCCESS(Urb->UrbHeader.Status)))
		pdx->LastFailedUrbStatus = Urb->UrbHeader.Status;

	// if ioStatus.Status indicates an error (ie. the IRP failed) then return that.
	// If ioStatus.Status indicates success, it is still possible that what we were
	// trying to do failed.  For example, if the IRP is cancelled, the status returned
	// by the I/O manager for the IRP will not indicate an error.  In that case, we
	// should check the URB status.  If it indicates anything other than
	// USBD_SUCCESS, then we should return STATUS_UNSUCCESSFUL.
	if(NT_SUCCESS(ntStatus))
	{
		if(!(USBD_SUCCESS(Urb->UrbHeader.Status)))
			ntStatus = STATUS_UNSUCCESSFUL;
	}

	KdPrint(("<= Stusb_CallUSBD (%x)\n", ntStatus));
	return ntStatus;
}


NTSTATUS
Stusb_ConfigureDevice(
    IN  PDEVICE_OBJECT fdo
    )
{
   PDEVICE_EXTENSION pdx;
   NTSTATUS ntStatus;
   PURB urb = NULL;
   ULONG siz;
   PUSB_CONFIGURATION_DESCRIPTOR configurationDescriptor = NULL;

   KdPrint (("=> Stusb_ConfigureDevice\n"));

   pdx = fdo->DeviceExtension;

   // Get memory for the USB Request Block (urb).
   urb = ExAllocatePoolWithTag(NonPagedPool,
                      sizeof(struct _URB_CONTROL_DESCRIPTOR_REQUEST),
                      ' dmW');
   if(urb != NULL)
   {
		// Set size of the data buffer.  Note we add padding to cover hardware faults
		// that may cause the device to go past the end of the data buffer
        siz = sizeof(USB_CONFIGURATION_DESCRIPTOR) + 16;
        // Get the nonpaged pool memory for the data buffer
        configurationDescriptor = ExAllocatePoolWithTag(NonPagedPool, 
									siz,
									' dmW');
        if(configurationDescriptor != NULL) 
        {
            UsbBuildGetDescriptorRequest(urb,
                                        (USHORT) sizeof (struct _URB_CONTROL_DESCRIPTOR_REQUEST),
                                         USB_CONFIGURATION_DESCRIPTOR_TYPE,
                                         0,
                                         0,
                                         configurationDescriptor,
                                         NULL,
                                         sizeof(USB_CONFIGURATION_DESCRIPTOR),/* Get only the configuration descriptor */
                                         NULL);

            ntStatus = Stusb_CallUSBD(fdo, urb);
            if(NT_SUCCESS(ntStatus)) 
            {
                KdPrint(("Configuration Descriptor is at %x, bytes txferred: %d\n\
                          Configuration Descriptor Actual Length: %d\n",
                          configurationDescriptor,
                          urb->UrbControlDescriptorRequest.TransferBufferLength,
                          configurationDescriptor->wTotalLength));
            }

        } 
        else 
        {
            ntStatus = STATUS_NO_MEMORY;
            goto Exit_StusbConfigureDevice;
        }

        // Determine how much data is in the entire configuration descriptor
        // and add extra room to protect against accidental overrun
        siz = configurationDescriptor->wTotalLength + 16;

        //  Free up the data buffer memory just used
        ExFreePool(configurationDescriptor);
        configurationDescriptor = NULL;

        // Get nonpaged pool memory for the data buffer
        configurationDescriptor = ExAllocatePoolWithTag(NonPagedPool, 
							siz,' dmW');
        // Now get the entire Configuration Descriptor
        if (configurationDescriptor != NULL) 
        {
            UsbBuildGetDescriptorRequest(urb,
                                         (USHORT)sizeof (struct _URB_CONTROL_DESCRIPTOR_REQUEST),
                                         USB_CONFIGURATION_DESCRIPTOR_TYPE,
                                         0,
                                         0,
                                         configurationDescriptor,
                                         NULL,
                                         siz,  // Get all the descriptor data
                                         NULL);

            ntStatus = Stusb_CallUSBD(fdo, urb);
            if(NT_SUCCESS(ntStatus)) 
            {
                KdPrint (("Entire Configuration Descriptor is at %x, bytes txferred: %d\n",
                          configurationDescriptor,
                          urb->UrbControlDescriptorRequest.TransferBufferLength));
            } 
            else 
            {
                //Error in getting configuration descriptor
                goto Exit_StusbConfigureDevice;
            }
        } 
        else 
        {
            // Failed getting data buffer (configurationDescriptor) memory
            ntStatus = STATUS_NO_MEMORY;
            goto Exit_StusbConfigureDevice;
        }
    } 
    else 
    {
        // failed getting urb memory
        ntStatus = STATUS_NO_MEMORY;
        goto Exit_StusbConfigureDevice;
    }

    // We have the configuration descriptor for the configuration
    // we want.
    // Now we issue the SelectConfiguration command to get
    // the  pipes associated with this configuration.
    if(configurationDescriptor) 
    {
        // Get our pipes
        ntStatus = Stusb_SelectInterfaces(fdo,
                                          configurationDescriptor,
                                          NULL // Device not yet configured
                                           );
    }

Exit_StusbConfigureDevice:

    // Clean up and exit this routine
    if(urb != NULL) 
    {
        ExFreePool(urb);                  
    }

    if(configurationDescriptor != NULL) 
    {
        ExFreePool(configurationDescriptor);
    }

    KdPrint (("<= Stusb_ConfigureDevice (%x)\n", ntStatus));
    return ntStatus;
}


NTSTATUS
Stusb_SelectInterfaces(
    IN PDEVICE_OBJECT fdo,
    IN PUSB_CONFIGURATION_DESCRIPTOR ConfigurationDescriptor,
    IN PUSBD_INTERFACE_INFORMATION Interface
    )
{
	PDEVICE_EXTENSION pdx;
	NTSTATUS ntStatus;
	PURB urb;
	ULONG j;
	UCHAR MyInterfaceNumber;
	PUSBD_INTERFACE_INFORMATION interfaceObject;
	USBD_INTERFACE_LIST_ENTRY   interfaceList[2];

	KdPrint (("=> Stusb_SelectInterfaces\n"));

	pdx = fdo->DeviceExtension;
	MyInterfaceNumber = SAMPLE_INTERFACE_NBR;

	// Search the configuration descriptor for the first interface/alternate 
	// setting
	interfaceList[0].InterfaceDescriptor =
		USBD_ParseConfigurationDescriptorEx(ConfigurationDescriptor,
									   ConfigurationDescriptor,
									   -1,         // Interface - don't care
									   -1,         // Alternate Setting - don't care
									   -1,         // Class - don't care
									   -1,         // SubClass - don't care
									   -1);        // Protocol - don't care

	ASSERT(interfaceList[0].InterfaceDescriptor != NULL);

	interfaceList[1].InterfaceDescriptor = NULL;
	interfaceList[1].Interface = NULL;

	urb = USBD_CreateConfigurationRequestEx(ConfigurationDescriptor,
											&interfaceList[0]);
	if(!urb)
	{
		KdPrint(("USBD_CreateConfigurationRequestEx failed\n"));
		ntStatus = STATUS_UNSUCCESSFUL;
		goto exit;        
	}

	interfaceObject = (PUSBD_INTERFACE_INFORMATION) (&(urb->UrbSelectConfiguration.Interface));

	// We set up a default max transfer size for the endpoints.  Your driver will
	// need to change this to reflect the capabilities of your device's endpoints.
	for(j=0; j<interfaceList[0].InterfaceDescriptor->bNumEndpoints; j++)
		interfaceObject->Pipes[j].MaximumTransferSize = (128*1024);
	ntStatus = Stusb_CallUSBD(fdo, urb);
	if(NT_SUCCESS(ntStatus) && USBD_SUCCESS(urb->UrbHeader.Status))
	{
		pdx->Configed = 1;

		// Save the configuration handle for this device
		pdx->ConfigurationHandle =
			urb->UrbSelectConfiguration.ConfigurationHandle;

		pdx->Interface = ExAllocatePoolWithTag(NonPagedPool,
									  interfaceObject->Length,
									  ' dmW');
		if(pdx->Interface != NULL)
		{
			// save a copy of the interfaceObject information returned
			RtlCopyMemory(pdx->Interface, interfaceObject, interfaceObject->Length);

			// Dump the interfaceObject to the debugger
			KdPrint (("---------\n"));
			KdPrint (("NumberOfPipes 0x%x\n", pdx->Interface->NumberOfPipes));
			KdPrint (("Length 0x%x\n", pdx->Interface->Length));
			KdPrint (("Alt Setting 0x%x\n", pdx->Interface->AlternateSetting));
			KdPrint (("Interface Number 0x%x\n", pdx->Interface->InterfaceNumber));

			// Dump the pipe info
			KdPrint (("---------\n"));
			for(j=0; j<interfaceObject->NumberOfPipes; j++)
			{
				PUSBD_PIPE_INFORMATION pipeInformation;
				pipeInformation = &pdx->Interface->Pipes[j];
				KdPrint (("***********************************\n"));
				KdPrint (("PipeType 0x%x\n", pipeInformation->PipeType));
				KdPrint (("EndpointAddress 0x%x\n", pipeInformation->EndpointAddress));
				KdPrint (("MaxPacketSize 0x%x\n", pipeInformation->MaximumPacketSize));
				KdPrint (("Interval 0x%x\n", pipeInformation->Interval));
				KdPrint (("Handle 0x%x\n", pipeInformation->PipeHandle));
				KdPrint (("MaximumTransferSize 0x%x\n", pipeInformation->MaximumTransferSize));
			}
			KdPrint (("---------\n"));
		}
		else
		{
			KdPrint(("Can not allocate memory!"));
			ntStatus = STATUS_NO_MEMORY;
		}
	}

	// add by me,Clean up and exit this routine  
	if(urb != NULL) 
	{
		ExFreePool(urb);                    
	}
exit:
	KdPrint(("<= Stusb_SelectInterfaces (%x)\n", ntStatus));
	return ntStatus;
}

NTSTATUS
Stusb_StopDevice(
   IN  PDEVICE_OBJECT fdo
   )
{
	PDEVICE_EXTENSION pdx;
	NTSTATUS ntStatus = STATUS_SUCCESS;
	PURB urb;
	ULONG siz;

	KdPrint (("=> Stusb_StopDevice\n"));

	pdx = fdo->DeviceExtension;

	// Send the select configuration urb with a NULL pointer for the configuration
	// handle, this closes the configuration and puts the device in the 'unconfigured'
	// state.
	siz = sizeof(struct _URB_SELECT_CONFIGURATION);
	urb = ExAllocatePoolWithTag(NonPagedPool,
						siz,' dmW');
	if(urb)
	{
		NTSTATUS status;
		UsbBuildSelectConfigurationRequest(urb,
					 (USHORT) siz,
					  NULL);
		status = Stusb_CallUSBD(fdo, urb);
		KdPrint(("Device Configuration Closed status = %x usb status = %x.\n",
			status, urb->UrbHeader.Status));
		ExFreePool(urb);
	}
	else
	{
		KdPrint(("Can not allocate memory!"));
		ntStatus = STATUS_NO_MEMORY;
	}

	KdPrint(("<= Stusb_StopDevice (%x)\n", ntStatus));
	return ntStatus;
}

NTSTATUS
Stusb_AbortPipe(
    IN PDEVICE_OBJECT fdo,
    IN USBD_PIPE_HANDLE PipeHandle
    )
{
	NTSTATUS ntStatus;
	PURB urb;

	KdPrint(("STUSB.SYS: Abort Pipe \n"));
	urb = ExAllocatePoolWithTag(NonPagedPool,
			  sizeof(struct _URB_PIPE_REQUEST),
			  ' dmW');
	if(urb)
	{
		RtlZeroMemory(urb,sizeof(struct _URB_PIPE_REQUEST));
		urb->UrbHeader.Length = (USHORT) sizeof (struct _URB_PIPE_REQUEST);
		urb->UrbHeader.Function = URB_FUNCTION_ABORT_PIPE;
		urb->UrbPipeRequest.PipeHandle = PipeHandle;
		ntStatus = Stusb_CallUSBD(fdo, urb);
		ExFreePool(urb);
	}
	else
	{
		ntStatus = STATUS_INSUFFICIENT_RESOURCES;
	}

	return ntStatus;
}


NTSTATUS
Stusb_ProcessIOCTL(
    IN PDEVICE_OBJECT fdo,
    IN PIRP Irp
    )
{
	PIO_STACK_LOCATION irpStack;
	PVOID ioBuffer;
	ULONG inputBufferLength;
	ULONG outputBufferLength;
	ULONG ioControlCode;
	NTSTATUS ntStatus;

	KdPrint(("=> IRP_MJ_DEVICE_CONTROL\n"));

	if(!LockDevice(fdo))
		return CompleteRequest(Irp, STATUS_DELETE_PENDING, 0);

	// Get a pointer to the current location in the Irp. This is where
	// the function codes and parameters are located.
	irpStack = IoGetCurrentIrpStackLocation (Irp);

	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	ioBuffer           = Irp->AssociatedIrp.SystemBuffer;
	inputBufferLength  = irpStack->Parameters.DeviceIoControl.InputBufferLength;
	outputBufferLength = irpStack->Parameters.DeviceIoControl.OutputBufferLength;

	ioControlCode = irpStack->Parameters.DeviceIoControl.IoControlCode;

	// Handle Ioctls from User mode
	switch(ioControlCode)
	{
		case IOCTL_STUSB_BULK_WRITE:
		case IOCTL_STUSB_BULK_READ:
		{
			Irp->IoStatus.Status = Stusb_Read_Write(fdo,Irp);
			break;
		}
		default:
			KdPrint(("Pass Down Ioctl %x\n",ioControlCode));
			Irp->IoStatus.Status = STATUS_NOT_SUPPORTED;
	}
	UnlockDevice(fdo);
	ntStatus = Irp->IoStatus.Status;
	IoCompleteRequest (Irp,IO_NO_INCREMENT);
	KdPrint(("<= IRP_MJ_DEVICE_CONTROL\n"));
	return ntStatus;
}

NTSTATUS
Stusb_Read_Write(
   IN  PDEVICE_OBJECT fdo,
   IN  PIRP Irp
   )
{
	PDEVICE_EXTENSION           pdx = fdo->DeviceExtension;
	NTSTATUS                    ntStatus;
	PIO_STACK_LOCATION          irpStack = IoGetCurrentIrpStackLocation(Irp);
	PBULK_TRANSFER_CONTROL      bulkControl =
							    (PBULK_TRANSFER_CONTROL)Irp->AssociatedIrp.SystemBuffer;
	ULONG                       bufferLength =
							    irpStack->Parameters.DeviceIoControl.OutputBufferLength;
	PURB                        urb = NULL;
	ULONG                       urbSize = 0;
	ULONG                       transferFlags = 0;
	PUSBD_INTERFACE_INFORMATION interfaceInfo = NULL;
	PUSBD_PIPE_INFORMATION      pipeInfo = NULL;
	USBD_PIPE_HANDLE            pipeHandle = NULL;


	KdPrint(("=> Stusb_Read_Write()\n"));

	// verify that the selected pipe is valid, and get a handle to it. 
	// If anything is wrong, return an error
	interfaceInfo = pdx->Interface;
	if(!interfaceInfo)
	{
		KdPrint(("Stusb_Read_Write() no interface info - Exiting\n"));
		return STATUS_UNSUCCESSFUL;
	}

	if(bulkControl->pipeNum > interfaceInfo->NumberOfPipes)
	{
		KdPrint(("Stusb_Read_Write() invalid pipe - Exiting\n"));
		return STATUS_INVALID_PARAMETER;
	}

	pipeInfo = &(interfaceInfo->Pipes[bulkControl->pipeNum]);
	if(!((pipeInfo->PipeType == UsbdPipeTypeBulk) ||
	     (pipeInfo->PipeType == UsbdPipeTypeInterrupt)))
	{
		KdPrint(("Stusb_Read_Write() invalid pipe - Exiting\n"));
		return STATUS_INVALID_PARAMETER;
	}

	pipeHandle = pipeInfo->PipeHandle;
	if(!pipeHandle)
	{
		KdPrint(("Stusb_Read_Write() invalid pipe - Exiting\n"));
		return STATUS_UNSUCCESSFUL;
	}

	if(bufferLength > pipeInfo->MaximumTransferSize)
	{
		KdPrint(("Stusb_Read_Write() invalid transfer size - Exiting\n"));
		return STATUS_INVALID_PARAMETER;
	}

	//allocate and fill in the Usb request (URB)
	urbSize = sizeof(struct _URB_BULK_OR_INTERRUPT_TRANSFER);
	urb = ExAllocatePoolWithTag(NonPagedPool,
								urbSize,' dmW');
	if(!urb)
	{
		KdPrint(("Stusb_Read_Write() unable to alloc URB - Exiting\n"));
		return STATUS_NO_MEMORY;
	}

	transferFlags = USBD_SHORT_TRANSFER_OK;
	// get direction info from the endpoint address
	if(USB_ENDPOINT_DIRECTION_IN(pipeInfo->EndpointAddress))
		transferFlags |= USBD_TRANSFER_DIRECTION_IN;
	UsbBuildInterruptOrBulkTransferRequest(urb,   //ptr to urb
						   (USHORT) urbSize,	  //size of urb
						   pipeHandle,            //usbd pipe handle
						   NULL,                  //TransferBuffer
						   Irp->MdlAddress,       //mdl
						   bufferLength,          //bufferlength
						   transferFlags,         //flags
						   NULL);                 //link

	// Call the USB Stack.
	ntStatus = Stusb_CallUSBD(fdo, urb);
	// If the transfer was successful, report the length of the transfer to the
	// caller by setting IoStatus.Information
	if(NT_SUCCESS(ntStatus))
	{
		Irp->IoStatus.Information = 
			urb->UrbBulkOrInterruptTransfer.TransferBufferLength;
		KdPrint(("Successfully transfered 0x%x bytes\n",
			Irp->IoStatus.Information));
	}
	ExFreePool(urb);
	KdPrint(("<= Stusb_Read_Write()\n"));
	return ntStatus;
}


NTSTATUS
Stusb_DispatchPower(
	IN PDEVICE_OBJECT fdo,
	IN PIRP Irp
	)
{
	PIO_STACK_LOCATION irpStack;
	PDEVICE_EXTENSION pdx = fdo->DeviceExtension;
	NTSTATUS ntStatus;

	KdPrint(("=> Stusb_DispatchPower\n"));

	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0;

	// Get a pointer to the current location in the Irp. This is where
	// the function codes and parameters are located.
	irpStack = IoGetCurrentIrpStackLocation (Irp);

	KdPrint(("IRP_MJ_POWER MIN=0x%x Type=0x%x State=0x%x\n",
		irpStack->MinorFunction,
		irpStack->Parameters.Power.Type,
		irpStack->Parameters.Power.State.DeviceState));

	IoCopyCurrentIrpStackLocationToNext(Irp);

	// All PNP_POWER messages get passed to the StackDeviceObject that
	// we were given in PnPAddDevice.
	// This stack device object is managed by the USB software subsystem,
	// and so this IRP must be propagated to the owning device driver for
	// that stack device object, so that driver in turn can perform any
	// device state management (e.g., remove its device object, etc.).
	KdPrint(("Passing Power Irp down\n"));

	PoStartNextPowerIrp(Irp);
	ntStatus = PoCallDriver(pdx->StackDeviceObject,Irp);

	// If lower layer driver marked the Irp as pending then reflect that by
	// calling IoMarkIrpPending.
	if(ntStatus == STATUS_PENDING)
	{
		IoMarkIrpPending(Irp);
		KdPrint(("Power Irp came back with STATUS_PENDING (%x)\n", ntStatus));
	}
	else
	{
		KdPrint(("Power Irp came back, status = %x\n", ntStatus));
	}
   
	KdPrint(("<= Stusb_DispatchPower %x\n", ntStatus));
	return ntStatus;
}

VOID
Stusb_ReleaseDeviceMem(
	IN PDEVICE_OBJECT fdo
	)
{
	PDEVICE_EXTENSION pdx;
	pdx = fdo->DeviceExtension;
	if(pdx->DeviceDescriptor)
	{
		ExFreePool(pdx->DeviceDescriptor);
		pdx->DeviceDescriptor = NULL;
	}
	// Free up any interface structures in our device extension
	if(pdx->Interface != NULL)
	{
		ExFreePool(pdx->Interface);
		pdx->Interface = NULL;
	}

	// remove the device object's symbolic link
	if(pdx->NeedCleanup)
	{
		UNICODE_STRING deviceLinkUnicodeString;
		RtlInitUnicodeString(&deviceLinkUnicodeString,
							 pdx->DeviceLinkNameBuffer);
		IoDeleteSymbolicLink(&deviceLinkUnicodeString);
		pdx->NeedCleanup = FALSE;
	}
}

VOID
Stusb_AbortAllPipes(
	IN PDEVICE_OBJECT fdo
	)
{
	ULONG i;
	PDEVICE_EXTENSION pdx;
	pdx = fdo->DeviceExtension;
	if(pdx->Interface)
	{
		for(i = 0; i < pdx->Interface->NumberOfPipes; i++)
		{
			Stusb_AbortPipe(fdo,
				(USBD_PIPE_HANDLE)pdx->Interface->Pipes[i].PipeHandle);
		}
	}
}
